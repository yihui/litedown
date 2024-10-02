#' A lightweight version of R Markdown
#'
#' Markdown is a plain-text format that can be converted to HTML and other
#' formats. This package can render R Markdown to Markdown, and then to an
#' output document format. The main differences between this package and
#' \pkg{rmarkdown} are that it does not use Pandoc or \pkg{knitr} (i.e., fewer
#' dependencies), and it also has fewer Markdown features.
#' @importFrom xfun alnum_id base64_uri csv_options download_cache fenced_block
#'   fenced_div file_exists file_ext grep_sub in_dir loadable new_record
#'   normalize_path prose_index raw_string read_all read_utf8 record_print
#'   Rscript_call sans_ext split_lines with_ext write_utf8
'_PACKAGE'

# an internal environment to store some intermediate objects
.env = new_env()

#' The `fuse()` environment
#'
#' Get the environment passed to the `envir` argument of [fuse()], i.e., the
#' environment in which code chunks and inline code are evaluated.
#' @return When called during `fuse()`, it returns the `envir` argument value of
#'   `fuse()`. When called outside `fuse()`, it returns the global environment.
#' @export
fuse_env = function() .env$global %||% globalenv()

#' @export
record_print.data.frame = function(x, ...) {
  asis = inherits(x, 'AsIs')
  if (is.null(getOption('xfun.md_table.limit')) && !'limit' %in% names(list(...))) {
    opts = options(xfun.md_table.limit = if (!asis) 10)
    on.exit(options(opts), add = TRUE)
  }
  if (asis) class(x) = setdiff(class(x), 'AsIs')
  if (inherits(x, 'tbl_df')) x = as.data.frame(x)
  tab = xfun::md_table(x, ...)
  opt = reactor()
  tab = add_cap(tab, opt$tab.cap, opt$label, opt$cap.pos %||% 'top', opt$tab.env, 'tab')
  new_record(c(tab, ''), 'asis')
}

#' @export
record_print.matrix = record_print.data.frame

#' @export
record_print.tbl_df = record_print.data.frame

#' @export
record_print.knitr_kable = function(x, ...) {
  if ((fmt <- attr(x, 'format')) %in% c('html', 'latex'))
    x = fenced_block(x, paste0('=', fmt))
  new_record(c(x, ''), 'asis')
}

# register vignette engines
.onLoad = function(lib, pkg) {
  vig_add('vignette', vig_fun(TRUE), vig_fun(FALSE))
  vig_add('book', vig_fun(TRUE), vig_fun(FALSE))
}

vig_add = function(name, weave, tangle) {
  tools::vignetteEngine(
    name, weave, tangle, '[.]R?md$', aspell = list(filter = vig_filter)
  )
}

# weave or tangle?
vig_fun = function(weave = TRUE, book = FALSE) {
  function(file, quiet = FALSE, ...) {
    empty_file = function() write_utf8(character(), with_ext(file, '.R'))

    # call fuse_book() to build multiple input files into a book
    if (book) return(if (weave) {
      if (quiet) {
        opt = options(litedown.progress.delay = Inf); on.exit(options(opt))
      }
      fuse_book(dirname(file), envir = globalenv())
    } else empty_file())

    # fuse() .Rmd and mark() .md
    if (grepl('[.]Rmd$', file)) {
      if (weave) fuse(file, quiet = quiet, envir = globalenv()) else {
        if (getRversion() <= '3.2.5') empty_file() else fiss(file)
      }
    } else {
      if (weave) mark(file) else empty_file()
    }
  }
}

# filter out code from document so aspell() won't spell check code
vig_filter = function(ifile, encoding) {
  res = crack(ifile)
  res = lapply(res, function(x) {
    if (x$type == 'code_chunk') return(rep('', length(x$source)))
    if (is.character(x$source)) x$source else {
      one_string(uapply(x$source, function(s) {
        if (is.character(s)) s else ''
      }), '')
    }
  })
  structure(split_lines(unlist(res)), control = '-H -t')
}

#' Get the package description, news, citation, and manual pages
#'
#' Helper functions to retrieve various types of package information that can be
#' put together as the full package documentation like a \pkg{pkgdown} website.
#' These functions can be called inside any R Markdown document.
#' @param name The package name (by default, it is automatically detected from
#'   the `DESCRIPTION` file if it exists in the current working directory or
#'   upper-level directories).
#' @return A character vector (HTML or Markdown) that will be printed as is
#'   inside a code chunk of an R Markdown document.
#'
#'   `pkg_desc()` returns an HTML table containing the package metadata.
#' @export
#' @examples
#' litedown::pkg_desc()
#' litedown::pkg_news()
#' litedown::pkg_citation()
#' litedown::pkg_manual()
pkg_desc = function(name = detect_pkg()) {
  d = packageDescription(name, fields = c(
    'Title', 'Version', 'Description', 'Depends', 'Imports', 'Suggests',
    'License', 'URL', 'BugReports', 'VignetteBuilder', 'Authors@R', 'Author'
  ))
  # remove single quotes on words (which are unnecessary IMO)
  for (i in c('Title', 'Description')) d[[i]] = sans_sq(d[[i]])
  # format authors
  if (is.na(d[['Author']])) d$Author = one_string(by = ',', format(
    eval(xfun::parse_only(d[['Authors@R']])), include = c('given', 'family', 'role')
  ))
  d[['Authors@R']] = NULL
  # convert URLs to <a>, and escape HTML in other fields
  for (i in names(d)) d[[i]] = if (!is.na(d[[i]])) {
    if (i %in% c('URL', 'BugReports')) {
      sans_p(commonmark::markdown_html(d[[i]], extensions = 'autolink'))
    } else xfun:::escape_html(d[[i]])
  }
  d = unlist(d)
  res = paste0(
    '<table class="table-full"><tbody>\n', paste0(
      '<tr>', paste0('\n<td>', names(d), '</td>'),
      paste0('\n<td>', d, '</td>'), '\n</tr>', collapse = '\n'
    ), '\n</tbody></table>'
  )
  new_asis(res)
}

#' @param path Path to the `NEWS.md` file. If empty, [news()] will be called to
#'   retrieve the news entries.
#' @param recent The number of recent versions to show. By default, only the
#'   latest version's news entries are retrieved. To show the full news, set
#'   `recent = 0`.
#' @param ... Other arguments to be passed to [news()].
#' @return `pkg_news()` returns the news entries.
#' @rdname pkg_desc
#' @export
pkg_news = function(name = detect_pkg(), path = detect_news(name), recent = 1, ...) {
  if (length(path) != 1 || path == '') {
    db = news(package = name, ...)
    if (recent > 0) db = head(db, recent)
    res = NULL
    for (v in unique(db$Version)) {
      df = db[db$Version == v, ]
      res = c(
        res, paste('## Changes in version', v, '{-}'), '',
        if (all(df$Category == '')) paste0(df$HTML, '\n') else paste0(
          '### ', df$Category, '{-}\n\n', df$HTML, '\n\n'
        ), ''
      )
    }
  } else {
    res = read_utf8(path)
    if (recent > 0 && length(h <- grep('^# ', res)) >= 2)
      res = res[h[1]:(h[1 + recent] - 1)]
    # lower heading levels: # -> ##, ## -> ###, etc, and unnumber them
    res = sub('^(## .+)', '#\\1 {-}', res)
    res = sub('^(# .+)', '#\\1 {-}', res)
  }
  new_asis(res)
}

#' @return `pkg_citation()` returns the package citation in both the plain-text
#'   and BibTeX formats.
#' @rdname pkg_desc
#' @export
pkg_citation = function(name = detect_pkg()) {
  res = uapply(citation(name), function(x) {
    x = tweak_citation(x)
    unname(c(format(x, bibtex = FALSE), fenced_block(toBibtex(x), 'latex')))
  })
  new_asis(res)
}

# dirty hack to add year if missing, and remove header
tweak_citation = function(x) {
  cls = class(x)
  x = unclass(x)
  attr(x[[1]], 'header') = attr(x, 'package') = NULL
  if (is.null(x[[1]]$year)) x[[1]]$year = format(Sys.Date(), '%Y')
  class(x) = cls
  x
}

#' @return `pkg_manual()` returns all manual pages of the package in HTML.
#' @rdname pkg_desc
#' @export
pkg_manual = function(name = detect_pkg()) {
  links = tools::findHTMLlinks('')
  # resolve internal links (will assign IDs of the form sec-man-ID to all h2)
  r = sprintf('^[.][.]/[.][.]/(%s)/html/(.+)[.]html$', name)
  i = grep(r, links)
  links[i] = paste0('#sec-man-', alnum_id(sub(r, '\\2', links[i])))
  # resolve external links to specific man pages on https://rdrr.io
  r = sprintf('^[.][.]/[.][.]/(%s)/html/', paste(xfun::base_pkgs(), collapse = '|'))
  links = sub(r, 'https://rdrr.io/r/\\1/', links)
  r = '^[.][.]/[.][.]/([^/]+)/html/'
  links = sub(r, 'https://rdrr.io/cran/\\1/man/', links)

  db = tools::Rd_db(name)  # all Rd pages
  al = lapply(db, Rd_aliases)

  # show the page name-package first
  idx = vapply(al, is.element, el = paste0(name, '-package'), FALSE)
  res = uapply(names(db)[order(!idx)], function(i) {
    txt = ''
    con = textConnection('txt', 'w', local = TRUE, encoding = 'UTF-8')
    tools::Rd2HTML(db[[i]], Links = links, out = con)
    close(con)
    txt = gsub('.*?(<h2[ |>].*)</main>.*', '\\1', one_string(txt))  # extract body
    sub('<h2>', sprintf('<h2 id="sec-man-%s">', alnum_id(al[[i]][1])), txt, fixed = TRUE)
  })

  # extract all aliases and put them in the beginning (like a TOC)
  env = asNamespace(name)
  toc = uapply(al, function(topics) {
    fn = uapply(topics, function(x) {
      if (is.function(env[[x]])) paste0(x, '()') else x  # add () after function names
    })
    sprintf('<a href="#sec-man-%s"><code>%s</code></a>', alnum_id(fn[1]), fn)
  })

  g = toupper(substr(unlist(al), 1, 1))
  g[!g %in% LETTERS] = 'misc'
  toc = split(toc, g)  # group by first character
  toc = unlist(mapply(function(x, g) {
    c('<p>', sprintf('<b>-- <kbd>%s</kbd> --</b>', g), x, '</p>')
  }, toc, names(toc)))

  res = gsub(" (id|class)='([^']+)'", ' \\1="\\2"', res)  # ' -> "
  res = gsub('<h3>', '<h3 class="unnumbered unlisted">', res, fixed = TRUE)
  res = gsub('<code id="[^"]+">', '<code>', res)
  res = gsub('(<code[^>]*>)\\s+', '\\1', res)
  res = gsub('\\s+(</code>)', '\\1', res)
  res = gsub('&#8288;', '', res, fixed = TRUE)
  res = gsub('<table>', '<table class="table-full">', res, fixed = TRUE)

  new_asis(c(toc, res))
}

detect_pkg = function() {
  # when running R CMD check, DESCRIPTION won't be under working directory
  root = if (!is.na(name <- xfun::check_package_name())) {
    root = file.path('00_pkg_src', name)
    if (dir.exists(root)) root
  }
  # if not found in R CMD check, locate DESCRIPTION via xfun::proj_root()
  if (is.null(root)) root = xfun::proj_root(rules = head(xfun::root_rules, 1))
  if (is.null(root)) stop(
    "Cannot automatically detect the package root directory from '", getwd(), "'. ",
    "You must provide the package name explicitly."
  )
  desc = read_utf8(file.path(root, 'DESCRIPTION'))
  name = grep_sub('^Package: (.+?)\\s*$', '\\1', desc)[1]
  structure(name, path = root)
}

detect_news = function(name) {
  if (isTRUE(file_exists(path <- file.path(attr(name, 'path'), 'NEWS.md'))))
    path else system.file('NEWS.md', package = name)
}

# get \alias{} names in an Rd object
Rd_aliases = function(x) {
  uapply(x, function(x) if (attr(x, 'Rd_tag') == '\\alias') as.character(x))
}
