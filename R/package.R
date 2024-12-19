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
  vig_add('book', vig_fun(TRUE, TRUE), vig_fun(FALSE, TRUE))
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

#' Print the package description, news, citation, manual pages, and source code
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
#' \dontrun{
#' litedown::pkg_desc()
#' litedown::pkg_news()
#' litedown::pkg_citation()
#' }
pkg_desc = function(name = detect_pkg()) {
  fields = c(
    'Title', 'Version', 'Description', 'Depends', 'Imports', 'Suggests',
    'License', 'URL', 'BugReports', 'VignetteBuilder', 'Authors@R', 'Author'
  )
  # read the DESCRIPTION file if pkg root is found, otherwise use installed info
  d = if (is.character(p <- attr(name, 'path'))) {
    as.list(read.dcf(file.path(p, 'DESCRIPTION'))[1, ][fields])
  } else {
    packageDescription(name, fields = fields)
  }
  names(d) = fields
  # remove single quotes on words (which are unnecessary IMO)
  for (i in c('Title', 'Description')) d[[i]] = sans_sq(d[[i]])
  d[['Author']] = one_string(pkg_authors(d), ', ')
  d[['Authors@R']] = NULL
  # convert URLs to <a>, and escape HTML in other fields
  for (i in names(d)) d[[i]] = if (!is.na(d[[i]])) {
    if (i %in% c('Description', 'URL', 'BugReports', 'Author')) {
      sans_p(commonmark::markdown_html(d[[i]], extensions = 'autolink'))
    } else xfun::html_escape(d[[i]])
  }
  d = unlist(d)
  res = paste0(
    '<table class="table-full"><tbody>\n', paste0(
      '<tr>', paste0('\n<td>', names(d), '</td>'),
      paste0('\n<td>', d, '</td>'), '\n</tr>', collapse = '\n'
    ), '\n</tbody></table>'
  )

  new_asis(c(res, fuel(css = '@manual')))
}

# format authors, adding URL and ORCID links as appropriate
pkg_authors = function(desc, role = NULL, extra = TRUE) {
  if (is.null(a <- desc[['Authors@R']])) return(desc[['Author']])
  a = eval(xfun::parse_only(a))
  a = uapply(a, function(x) {
    if (length(role) && !any(role %in% x$role)) return()
    role = if (extra && length(x$role)) paste0('[', one_string(x$role, ', '), ']')
    name = paste(x$given, x$family)
    comment = as.list(x$comment)
    orcid = if (extra) sprintf(
      '[![ORCID iD](https://cloud.r-project.org/web/orcid.svg){.orcid}](https://orcid.org/%s)',
      comment[["ORCID"]]
    )
    link = comment[['URL']]
    if (length(link)) name = sprintf('[%s](%s)', name, link)
    one_string(c(name, orcid, role), ' ')
  })
  a
}

#' @param path For [pkg_news()], path to the `NEWS.md` file. If empty, [news()]
#'   will be called to retrieve the news entries. For [pkg_code()], path to the
#'   package root directory that contains `R/` and/or `src/` subdirectories.
#' @param recent The number of recent versions to show. By default, only the
#'   latest version's news entries are retrieved. To show the full news, set
#'   `recent = 0`.
#' @param toc Whether to add section headings to the document TOC (when TOC has
#'   been enabled for the document).
#' @param number_sections Whether to number section headings (when sections are
#'   numbered in the document).
#' @param ... Other arguments to be passed to [news()].
#' @return `pkg_news()` returns the news entries.
#' @rdname pkg_desc
#' @export
pkg_news = function(
  name = detect_pkg(), path = detect_news(name), recent = 1, toc = TRUE,
  number_sections = TRUE, ...
) {
  a = header_class(toc, number_sections)
  if (length(path) != 1 || path == '') {
    db = news(package = name, ...)
    if (recent > 0) db = head(db, recent)
    res = NULL
    for (v in unique(db$Version)) {
      df = db[db$Version == v, ]
      res = c(
        res, paste('##', name, v, a), '',
        if (all(df$Category == '')) paste0(df$HTML, '\n') else paste0(
          '### ', df$Category, a, '\n\n', df$HTML, '\n\n'
        ), ''
      )
    }
  } else {
    res = read_utf8(path)
    if (recent > 0 && length(h <- grep('^# ', res)) >= 2)
      res = res[h[1]:(h[1 + recent] - 1)]
    # lower heading levels: # -> ##, ## -> ###, etc, and add attributes
    for (i in 2:1) res = sub(sprintf('^(#{%d} .+)', i), paste0('#\\1', a), res)
    # shorten headings
    res = gsub('^## CHANGES IN ([^ ]+) VERSION( .+)', '## \\1\\2', res)
  }
  new_asis(res)
}

# classes for section headings in news, code, and manual
header_class = function(toc, number_sections, md = TRUE) {
  a = c(if (!toc) 'unlisted', if (!number_sections) 'unnumbered')
  if (md && length(a)) a = paste0('.', a)
  a = one_string(a, ' ')
  if (a != '') a = if (md) paste0(' {', a, '}') else paste0(' class="', a, '"')
  a
}

#' @param pattern A regular expression to match filenames that should be treated
#'   as source code.
#' @param link Whether to add links on the file paths of source code. By
#'   default, if a GitHub repo link is detected from the `BugReports` field of
#'   the package `DESCRIPTION`, GitHub links will be added to file paths. You
#'   can also provide a string template containing the placeholder `%s` (which
#'   will be filled out with the file paths via `sprintf()`), e.g.,
#'   `https://github.com/yihui/litedown/blob/main/%s`.
#' @return `pkg_code()` returns the package source code under the `R/` and
#'   `src/` directories.
#' @rdname pkg_desc
#' @export
pkg_code = function(
  path = attr(detect_pkg(), 'path'), pattern = '[.](R|c|h|f|cpp)$', toc = TRUE,
  number_sections = TRUE, link = TRUE
) {
  if (!isTRUE(dir.exists(path))) return()
  a = header_class(toc, number_sections)
  if (isTRUE(link)) {
    u = read.dcf(file.path(path, 'DESCRIPTION'), 'BugReports')[1, 1]
    u = grep_sub('^(https://github.com/[^/]+/[^/]+/).*', '\\1blob/HEAD/%s', u)
    if (length(u)) link = u
  }
  ds = c('R', 'src')
  ds = ds[ds %in% list.dirs(path, FALSE, FALSE)]
  flat = length(ds) == 1  # if only one dir exists, list files in a flat structure
  code = in_dir(path, lapply(ds, function(d) {
    fs = list.files(d, pattern, full.names = TRUE, recursive = TRUE)
    if (length(fs) == 0) return()
    x = uapply(fs, function(f) c(
      sprintf('##%s %s%s', if (flat) '' else '#', if (is.character(link)) {
        sprintf('[`%s`](%s)', f, sprintf(link, f))
      } else sprintf('`%s`', f), a), '',
      fenced_block(read_utf8(f), lineno_attr(file_ext(f), auto = FALSE)), ''
    ))
    e = unique(file_ext(fs))
    c(if (!flat) paste0('## ', paste0('`*.', e, '`', collapse = ' / '), a), '', x)
  }))
  new_asis(unlist(code))
}

#' @return `pkg_citation()` returns the package citation in both the plain-text
#'   and BibTeX formats.
#' @rdname pkg_desc
#' @export
pkg_citation = function(name = detect_pkg()) {
  res = uapply(citation(name), function(x) {
    x = tweak_citation(x)
    unname(c(format(x, style = 'text'), fenced_block(toBibtex(x), 'latex')))
  })
  new_asis(res)
}

# dirty hack to add year if missing
tweak_citation = function(x) {
  cls = class(x)
  x = unclass(x)
  if (is.null(x[[1]]$year)) x[[1]]$year = format(Sys.Date(), '%Y')
  class(x) = cls
  x
}

#' @param overview Whether to include the package overview page, i.e., the
#'   `{name}-package.Rd` page.
#' @param examples A list of arguments to be passed to [xfun::record()] to run
#'   examples each help page, e.g., `list(dev = 'svg', dev.args = list(height =
#'   6))`. If not a list (e.g., `FALSE`), examples will not be run.
#' @return `pkg_manual()` returns all manual pages of the package in HTML.
#' @rdname pkg_desc
#' @export
pkg_manual = function(
  name = detect_pkg(), toc = TRUE, number_sections = TRUE, overview = TRUE,
  examples = list()
) {
  links = tools::findHTMLlinks('')
  # resolve internal links (will assign IDs of the form sec:man-ID to all h2)
  r = sprintf('^[.][.]/[.][.]/(%s)/html/(.+)[.]html$', name)
  i = grep(r, links)
  links[i] = paste0('#sec:man-', alnum_id(sub(r, '\\2', links[i])))
  # resolve external links to specific man pages on https://rdrr.io
  r = sprintf('^[.][.]/[.][.]/(%s)/html/', paste(xfun::base_pkgs(), collapse = '|'))
  links = sub(r, 'https://rdrr.io/r/\\1/', links)
  r = '^[.][.]/[.][.]/([^/]+)/html/'
  links = sub(r, 'https://rdrr.io/cran/\\1/man/', links)

  db = tools::Rd_db(name)  # all Rd pages
  intro = paste0(name, '-package.Rd')  # the name-package entry (package overview)
  entries = setdiff(names(db), intro)
  db = db[c(if (overview && intro %in% names(db)) intro, entries)]
  al = lapply(db, Rd_aliases)

  cl = header_class(toc, number_sections, FALSE)
  r1 = '<code class="reqn">\\s*([^<]+?)\\s*</code>'  # inline math
  r2 = sprintf('<p[^>]*>\\s*%s\\s*</p>', r1)  # display math
  res = uapply(names(db), function(i) {
    txt = ''
    con = textConnection('txt', 'w', local = TRUE, encoding = 'UTF-8')
    tryCatch(
      tools::Rd2HTML(db[[i]], Links = links, out = con), error = function(e) {
        warning("The Rd file '", i, "' appears to be malformed.", call. = FALSE)
        stop(e)
      }, finally = close(con))
    # extract body, which may end at </main> (R 4.4.x) or </div></body> (R 4.3.x)
    txt = gsub('(?s).*?(?=<h2)', '', one_string(txt), perl = TRUE)
    txt = gsub('(</main>|</div>\\s*</body>).*', '', txt)
    # free math from <code>
    txt = gsub(r2, '<p>$$\\1$$</p>', txt)
    txt = gsub(r1, '<span>\\\\(\\1\\\\)</span>', txt)
    # run examples
    if (is.list(examples)) {
      xfun::pkg_attach(name)
      default = list(print = NA, dev.path = 'manual/', dev.args = list(width = 9, height = 7))
      txt = run_examples(txt, merge_list(default, examples), sans_ext(i))
    }
    # remove existing ID and class
    for (a in c('id', 'class')) txt = gsub(sprintf('(<h2[^>]*?) %s="[^"]+"', a), '\\1', txt)
    if (cl != '') txt = sub('<h2', paste0('<h2', cl), txt, fixed = TRUE)
    sub('<h2', sprintf('<h2 id="sec:man-%s"', alnum_id(al[[i]][1])), txt, fixed = TRUE)
  })

  # extract all aliases and put them in the beginning (like a TOC)
  env = asNamespace(name)
  toc = uapply(al, function(topics) {
    fn = uapply(topics, function(x) {
      if (is.function(env[[x]])) paste0(x, '()') else x  # add () after function names
    })
    sprintf('<a href="#sec:man-%s"><code>%s</code></a>', alnum_id(fn[1]), fn)
  })

  g = toupper(substr(unlist(al), 1, 1))
  g[!g %in% LETTERS] = 'misc'
  g = factor(g, intersect(c(LETTERS, 'misc'), g))
  toc = split(toc, g)  # group by first character
  toc = unlist(mapply(function(x, g) {
    c('<p>', sprintf('<b>-- <kbd>%s</kbd> --</b>', g), x, '</p>')
  }, toc, names(toc)))

  res = gsub(" (id|class)='([^']+)'", ' \\1="\\2"', res)  # ' -> "
  res = gsub('<h3>', '<h3 class="unnumbered unlisted">', res, fixed = TRUE)
  res = gsub('<code id="[^"]+">', '<code>', res)
  res = gsub('(<code[^>]*>)\\s+', '\\1', res)
  res = gsub('\\s+(</code>)', '\\1', res)
  res = gsub('<div class="sourceCode"><pre>(.+?)</pre></div>', '<pre><code>\\1</code></pre>', res)
  res = gsub('<div class="sourceCode ([^"]+)"><pre>(.+?)</pre></div>', '<pre><code class="language-\\1">\\2</code></pre>', res)
  res = gsub('<code class="language-R"', '<code class="language-r"', res, fixed = TRUE)
  res = gsub('&#8288;', '', res, fixed = TRUE)
  res = gsub('<table>', '<table class="table-full">', res, fixed = TRUE)
  new_asis(c(toc, res, fuel(css = '@manual')))
}

run_examples = function(html, config, path) {
  config$dev.path = path = paste0(config$dev.path, path)
  on.exit(xfun::del_empty_dir(dirname(path)), add = TRUE)
  r = '(?s).*?<pre><code[^>]*>(?s)(.+?)</code></pre>'
  match_replace(html, paste0('(?<=<h3>Examples</h3>)', r), function(x) {
    code = gsub(r, '\\1', x, perl = TRUE)
    code = restore_html(str_trim(code))
    nr1 = 'if (FALSE) {  ## Not run'
    nr2 = '}  ## Not run'
    code = gsub('\n?## Not run:\\s*?\n', paste0('\n', nr1, '\n'), code)
    code = gsub('\n+## End[(]Not run[)]\n*', paste0('\n', nr2, '\n'), code)
    res = do.call(xfun::record, merge_list(config, list(code = code, envir = globalenv())))
    idx = seq_along(res); cls = class(res)
    for (i in idx) {
      ri = res[[i]]; ci = class(ri)
      # disable asis output since it may contain raw HTML
      if ('record_asis' %in% ci) class(res[[i]]) = 'record_output'
      # split the dontrun block
      if ('record_source' %in% ci && !any(is.na(nr <- match(c(nr1, nr2), ri)))) {
        i1 = nr[1]; i2 = nr[2]
        new_block = function(i, ...) {
          b = trim_blank(one_string(ri[i]))
          if (xfun::is_blank(b)) b = character()
          list(structure(b, class = c(ci, ...)))
        }
        if (i1 > 1) {
          res = c(res, new_block((i1 + 1):(i2 - 1), 'fade'))
          idx = c(idx, i)
        }
        n = length(ri)
        if (i2 < n) {
          res = c(res, new_block((i2 + 1):n))
          idx = c(idx, i)
        }
        res[i] = if (i1 > 1) new_block(1:(i1 - 1)) else
          new_block((i1 + 1):(i2 - 1), 'fade')
      }
    }
    res = res[order(idx)]
    class(res) = cls
    res = one_string(c('', format(res, 'markdown')))
    res
  })
}

detect_pkg = function(error = TRUE) {
  ds = if (xfun::is_R_CMD_check()) {
    # R CMD check's working directory is PKG_NAME.Rcheck by default
    name = grep_sub('[.]Rcheck$', '', basename(getwd()))
    # when running R CMD check, DESCRIPTION won't be under working directory but
    # ../PKG_NAME/ (on CRAN's *nix) or ./00_pkg_src/
    c(file.path('..', name), if (dir.exists('00_pkg_src'))
      dirname(list.files('.', '^DESCRIPTION$', recursive = TRUE)))
  } else name = NULL
  for (d in c(ds, './')) {
    if (!is.null(root <- xfun::proj_root(d, head(xfun::root_rules, 1)))) break
  }
  if (is.null(root)) {
    root = if (length(name)) system.file(package = name)
    if (identical(root, '')) root = NULL
  }
  if (is.null(root)) {
    if (error) stop(
      "Cannot automatically detect the package root directory from '", getwd(), "'. ",
      "You must provide the package name explicitly."
    ) else return()
  } else if (is.null(name)) {
    desc = read_utf8(file.path(root, 'DESCRIPTION'))
    name = grep_sub('^Package: (.+?)\\s*$', '\\1', desc)[1]
  }
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
