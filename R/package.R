#' A lightweight version of R Markdown
#'
#' Markdown is a plain-text format that can be converted to HTML and other
#' formats. This package can render R Markdown to Markdown, and then to an
#' output document format. The main differences between this package and
#' \pkg{rmarkdown} are that it does not use Pandoc or \pkg{knitr} (i.e., fewer
#' dependencies), and it also has fewer Markdown features.
#' @importFrom xfun alnum_id base64_uri csv_options del_empty_dir dir_create
#'   divide_chunk download_cache exit_call fenced_block fenced_div file_exists
#'   file_ext grep_sub html_escape html_tag html_value in_dir is_abs_path
#'   is_blank is_rel_path loadable mime_type new_app new_record normalize_path
#'   parse_only raw_string read_all read_utf8 record_print
#'   relative_path Rscript_call same_path sans_ext set_envvar split_lines
#'   try_error try_silent with_ext write_utf8
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
#' @param type The HTML tag for the description: `table` (`<table>` with `<tr>`
#'   and `<td>`) or `dl` (definition list `<dl>` with `<dt>` and `<dd>`). The
#'   `dl` type generates a compact description for a sidebar (a fenced Div with
#'   the classes `side` and `side-right`): the fields `Description` and
#'   `Suggests` are omitted, and the fields `URL` and `BugReports` are merged
#'   into a field `Links`, in which URLs are shown as short text links (instead
#'   of long bare URLs that cannot be wrapped in a narrow sidebar).
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
pkg_desc = function(name = detect_pkg(), type = c('table', 'dl')) {
  type = match.arg(type)
  fields = c(
    'Title', 'Version', 'Description', 'Depends', 'Imports', 'Suggests',
    'License', 'URL', 'BugReports', 'VignetteBuilder', 'Authors@R', 'Author'
  )
  d = read_desc(name, fields)
  names(d) = fields
  # remove single quotes on words (which are unnecessary IMO)
  for (i in c('Title', 'Description')) d[[i]] = sans_sq(d[[i]])
  a_all = pkg_authors(d)
  # in the sidebar, show only authors by default when the list is long, and
  # fold the others
  a_aut = if (type == 'dl' && length(a_all) > 5) pkg_authors(d, 'aut')
  a_rest = if (length(a_aut)) pkg_authors(d, 'aut', negate = TRUE)
  d[['Author']] = one_string(a_all, ', ')
  d[['Authors@R']] = NULL
  if (type == 'dl') {
    # reuse the URL slot (to keep the field order) for all links
    d[['URL']] = side_links(d[['URL']], d[['BugReports']], name)
    d[c('Title', 'Description', 'BugReports', 'Suggests')] = NULL
    names(d)[names(d) == 'URL'] = 'Links'
  }
  # convert URLs to <a>, and escape HTML in other fields
  for (i in names(d)) d[[i]] = if (!is.na(d[[i]])) {
    if (i %in% c('Description', 'URL', 'BugReports', 'Links', 'Author')) {
      md_inline(d[[i]])
    } else html_escape(d[[i]])
  }
  if (length(a_aut) && length(a_rest)) d[['Author']] = sprintf(
    '%s\n<details><summary>Other contributors</summary>%s</details>',
    md_inline(one_string(a_aut, ', ')), md_inline(one_string(a_rest, ', '))
  )
  d = unlist(d)
  res = one_string(switch(
    type,
    table = c(
      '<table class="desc-table"><tbody>',
      sprintf('<tr>\n<td>%s</td>\n<td>%s</td>\n</tr>', names(d), d),
      '</tbody></table>'
    ),
    dl = c('<dl class="pkg-desc">', sprintf('<dt>%s</dt>\n<dd>%s</dd>', names(d), d), '</dl>')
  ))
  # put the sidebar in a fenced Div (blank lines separate it from raw HTML)
  if (type == 'dl') res = c('::: {.side .side-right}', '', res, '', ':::')
  new_asis(c(res, vest(css = '@manual')))
}

# Markdown to inline HTML, with URLs converted to links
md_inline = function(x) sans_p(markdown_html(x, extensions = 'autolink'))

# is a URL the site of the package itself (the site config `url`, or the
# conventional https://USER.github.io/PKG/)? Such links are pointless on the site.
is_own_site = function(u, pkg) {
  norm = function(x) tolower(sub('/+$', '', x))
  own = norm(yml_config('.')[['site']][['url']])
  r = if (length(pkg)) grepl(
    sprintf('^https?://[^/]+[.]github[.]io/%s$', tolower(pkg)), norm(u)
  ) else FALSE
  norm(u) %in% own | r
}

# turn URLs into Markdown links with short labels
side_links = function(url, bug, pkg = NULL) {
  u = if (!is.na(url)) match_full(url, 'https?://[^[:space:],<>()]+')
  u = u[!is_own_site(u, pkg)]
  host = sub('^https?://(www[.])?([^/]+).*', '\\2', u)
  repo = host %in% c(
    'github.com', 'gitlab.com', 'codeberg.org', 'bitbucket.org', 'dev.azure.com'
  )
  x = c(
    sprintf('[%s](%s)', ifelse(repo, 'Browse source code', host), u),
    if (!is.na(bug)) sprintf('[Report a bug](%s)', bug)
  )
  if (length(x)) one_string(x, '\\\n')
}

# format authors, adding URL and ORCID links as appropriate
# (`role`: keep only people with any of the roles, or the others if `negate`)
pkg_authors = function(desc, role = NULL, extra = TRUE, negate = FALSE) {
  if (is.null(a <- desc[['Authors@R']])) return(desc[['Author']])
  a = eval(parse_only(a))
  a = uapply(a, function(x) {
    if (length(role) && any(role %in% x$role) == negate) return()
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
    # link Github @username and #issue
    if (length(u <- github_link(dirname(path)))) {
      # the link may follow a space or an opening parenthesis
      r0 = '(^|[ (])'; r1 = '([[:alnum:]-]+)'; r2 = '([0-9]+)'
      res = gsub(paste0(r0, '@', r1), '\\1[@\\2](https://github.com/\\2)', res)
      res = gsub(paste0(r0, '#', r2), sprintf('\\1[#\\2](%sissues/\\2)', u), res)
      res = gsub(
        sprintf('%s%s/%s#%s', r0, r1, r1, r2),
        '\\1[\\2/\\3#\\4](https://github.com/\\2/\\3/issues/\\4)', res
      )
    }
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
  if (!isTRUE(dir.exists(path))) {
    if (missing(path)) message('Please run this function in the package source directory.')
    return()
  }
  a = header_class(toc, number_sections)
  if (isTRUE(link) && length(u <- github_link(path))) link = paste0(u, 'blob/HEAD/%s')
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

#' @param dir The directory of examples, relative to the package root `path`.
#' @return `pkg_examples()` returns a list of boxes for the example files (e.g.,
#'   `.Rmd` files) under the `examples/` directory of the package, each with a
#'   link to its output, a link to its source, and a preview of the source.
#' @rdname pkg_desc
#' @export
pkg_examples = function(
  path = attr(detect_pkg(), 'path'), dir = 'examples', groups = site_groups('examples')
) {
  d = file.path(path, dir)
  if (!isTRUE(dir.exists(d))) {
    if (missing(path)) message('Please run this function in the package source directory.')
    return(new_asis(sprintf("The '%s/' directory does not exist.", dir)))
  }
  # if the examples have been copied to the working directory (the site dir), link
  # to the copies; otherwise link to the originals
  base = if (dir.exists(dir)) dir else relative_path(d)
  res = lapply(find_input(d), function(f) {
    b = basename(f); f2 = file.path(base, b)
    # in roam(), keep the local link so that the file can be opened in an editor
    src = (if (!is_roaming()) github_file_url(path, file.path(dir, b))) %||% f2
    # in roam(), the output is rendered on the fly from the source
    h = if (is_roaming()) paste0(f2, '?preview=2') else with_ext(f2, 'html')
    x = read_utf8(f)
    # get title
    t = md_title(x, b)
    # get example number
    i = grep_sub('^([0-9]+).*', '\\1', b)
    i = if (length(i)) paste0(i, '\\. ') else ''
    # put example source in a box (and keep the names to match groups)
    structure(fenced_div(c(
      fenced_div(
        sprintf('%s[%s](%s) _(source: [%s](%s))_ [&#9658;](%s){.run}', i, t, h, b, src, h),
        '.caption .name'
      ),
      fenced_block(x, '.md')
    ), '.box'), keys = c(sans_ext(b), b, t))
  })
  # the boxes are laid out in a grid (see listing.css)
  new_asis(if (length(groups)) {
    group_blocks(res, lapply(res, attr, 'keys'), groups, '.boxes')
  } else fenced_div(unlist(res), '.boxes'))
}

# retrieve Github repo link from DESCRIPTION; pkg_site() stores it in an env var
# for the package it builds, because each page of the site may be rendered in a
# new R session, where an in-memory cache would be lost
github_link = function(path) {
  if (nzchar(p <- Sys.getenv('R_LITEDOWN_PKG')) && same_path(path, p) &&
    !is.na(u <- Sys.getenv('R_LITEDOWN_GITHUB', NA))) return(u[nzchar(u)])
  u = read.dcf(file.path(path, 'DESCRIPTION'), 'BugReports')[1, 1]
  grep_sub('^(https://github.com/[^/]+/[^/]+/).*', '\\1', u)
}

# the branch of the GitHub Actions run (the source branch for a pull request),
# falling back to main
gha_branch = function() {
  b = Sys.getenv('GITHUB_HEAD_REF')  # only set for pull requests
  if (b == '' && Sys.getenv('GITHUB_REF_TYPE') == 'branch')
    b = Sys.getenv('GITHUB_REF_NAME')
  if (b == '') 'main' else b
}

# the GitHub URL of `file` (relative to the package root `path`), if the package
# has a GitHub repo and the file exists in the package: the edit page if `edit =
# TRUE, which needs a branch name since HEAD doesn't work there),
# otherwise the file view (.md files need ?plain=1 to show the source)
github_file_url = function(path, file, edit = FALSE) {
  if (all(file_exists(file.path(path, c(file, 'DESCRIPTION')))) &&
    length(u <- github_link(path))) paste0(
    u, if (edit) paste0('edit/', gha_branch()) else 'blob/HEAD', '/', file,
    if (!edit && grepl('[.]md$', file)) '?plain=1'
  )
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
#' @param groups A list of groups, each of which is a list with the fields
#'   `title`, `desc` (optional), and `contents`, where `contents` is a character
#'   vector of names to be matched (`*` and `?` are wildcards): topic names or
#'   Rd filenames for `pkg_manual()`, vignette filenames or titles for
#'   `pkg_articles()`, and filenames or titles of the example files for
#'   `pkg_examples()`. Items that do not belong to any group are put in a group
#'   named `Other`, except help pages with the keyword `internal`, which are
#'   listed only if a group names them (e.g., re-exported functions). A name
#'   that matches nothing is an error. By default, groups are read from the
#'   subfield `manual`, `articles`, or `examples` of the field `site` in
#'   `_litedown.yml` in the current directory, e.g.,
#'
#' ```yaml
#' site:
#'   manual:
#'     Sites:
#'       desc: Build package sites
#'       contents: [pkg_site, "roam*"]
#'     Others: [mark, fuse]
#' ```
#'
#'   That is, the keys are group titles, and each value is either the
#'   `contents` or a map with `desc` and `contents`.
#'
#'   If no groups are provided, `pkg_manual()` lists topics alphabetically, and
#'   `pkg_articles()` and `pkg_examples()` list all items in a single list.
#' @param full Whether to put all help pages on a single page. If `FALSE`,
#'   `pkg_manual()` only returns the index, a list with one bullet per help
#'   page of the form `fn1(), fn2(): title` (the list is divided by `groups`
#'   if provided), in which the function names link to the page under
#'   `manual/`. [pkg_site()] writes these pages
#'   before it renders the site, so the default is `FALSE` only when a site is
#'   being built by it (unless `full_manual: true` is set under `site` in
#'   `_litedown.yml`), and `TRUE` otherwise.
#' @param topic The name of a help page (an Rd filename, with or without
#'   `.Rd`). If provided, only the body of this page is returned, with links to
#'   other pages for use in a separate page (see `full`).
#' @return `pkg_manual()` returns all manual pages of the package in HTML.
#' @rdname pkg_desc
#' @export
pkg_manual = function(
  name = detect_pkg(), toc = TRUE, number_sections = TRUE, overview = TRUE,
  examples = list(), groups = site_groups('manual'),
  full = manual_full(), topic = NULL
) {
  m = manual_db(name, groups, overview)
  db = m$db; al = m$al; groups = m$groups; intro = m$intro
  if (!is.null(topic)) {
    if (is.na(i <- match(manual_file(topic), manual_file(names(db))))) stop(
      "No help page '", topic, "' in the package (or excluded by the groups)."
    )
    db = db[i]
  }
  # `full = FALSE`: this call only builds the index, which links to the pages of
  # topics (one file per topic under manual/, written by pkg_site() before the
  # site is rendered); `topic`: this call builds the body of one such page
  index_only = !isTRUE(full) && is.null(topic)
  paged = index_only || !is.null(topic)
  links = tools::findHTMLlinks('')
  # internal links: IDs of the form sec:man-ID (assigned to all h2) in a single
  # page, or pages in the same directory; prefer this package's aliases over
  # identically named aliases from other installed packages
  f = sans_ext(names(al))
  target = if (paged) paste0(alnum_id(f), '.html') else paste0('#sec:man-', f)
  links[unlist(al, use.names = FALSE)] = rep(target, lengths(al))
  # resolve external links to specific man pages on https://rdrr.io
  r = sprintf('^[.][.]/[.][.]/(%s)/html/', paste(xfun::base_pkgs(), collapse = '|'))
  links = sub(r, 'https://rdrr.io/r/\\1/', links)

  cl = header_class(toc, number_sections, FALSE)
  r1 = '<code class="reqn">\\s*([^<]+?)\\s*</code>'  # inline math
  r2 = sprintf('<p[^>]*>\\s*%s\\s*</p>', r1)  # display math
  res = if (index_only) character() else uapply(names(db), function(i) {
    txt = rd2html(db[[i]], links, i)
    # free math from <code>
    txt = gsub(r2, '<p>$$\\1$$</p>', txt)
    txt = gsub(r1, '<span>\\\\(\\1\\\\)</span>', txt)
    # run examples
    if (is.list(examples)) {
      xfun::pkg_attach(name)
      default = list(print = function(x, ...) {
        if (inherits(x, 'xfun_raw_string')) record_print(x) else
          xfun:::record_print.default(x)
      }, dev.path = if (is.null(topic)) 'manual/' else '', dev.args = list(width = 9, height = 7))
      txt = run_examples(txt, merge_list(default, examples), sans_ext(i))
    }
    # remove existing ID and class
    for (a in c('id', 'class')) txt = gsub(sprintf('(<h2[^>]*?) %s="[^"]+"', a), '\\1', txt)
    if (cl != '') txt = sub('<h2', paste0('<h2', cl), txt, fixed = TRUE)
    sub('<h2', sprintf('<h2 id="sec:man-%s"', sans_ext(i)), txt, fixed = TRUE)
  })

  # extract all aliases and put them in the beginning (like a TOC)
  toc = if (is.null(topic)) {
    env = asNamespace(name)
    toc = lapply(al, function(topics) {
      uapply(topics, function(x) {
        if (is.function(env[[x]])) paste0(x, '()') else x  # add () after function names
      })
    })
    # one bullet per help page: `[fn1(), fn2()](page): title`
    toc_items = function(ids) {
      t = vapply(ids, function(i) rd_title(rd2html(db[[i]], links, i)), '')
      fn = vapply(ids, function(i) paste(sprintf('`%s`', toc[[i]]), collapse = ', '), '')
      sprintf('- [%s](manual/%s.html): %s', fn, manual_file(ids), t)
    }
    toc_links = function(toc) {
      unlist(.mapply(function(fn, target) {
        sprintf('<a href="%s"><code>%s</code></a>', target, fn)
      }, toc, if (paged) paste0('manual/', manual_file(names(toc)), '.html') else
        paste0('#sec:man-', sans_ext(names(toc)))))
    }
    toc = if (length(groups)) {
      ov = intersect(intro, names(db))
      unlist(use.names = FALSE, lapply(c(
        if (length(ov)) list(list(title = 'Overview', i = ov)), groups
      ), function(g) {
        if (index_only) toc_block(toc_items(g$i), g$title, g$desc, TRUE) else
          toc_block(toc_links(toc[g$i]), g$title, g$desc)
      }))
    } else if (index_only) {
      toc_items(names(db))
    } else {
      # group by first character of aliases
      g = toupper(substr(unlist(al), 1, 1))
      g[!g %in% LETTERS] = 'misc'
      g = factor(g, intersect(c(LETTERS, 'misc'), g))
      l = split(toc_links(toc), g)
      unlist(lapply(names(l), function(i) {
        toc_block(c(sprintf('<b>-- <kbd>%s</kbd> --</b>', i), l[[i]]))
      }), use.names = FALSE)
    }
    # put the index in a callout (blank lines make it a fenced Div, not HTML),
    # unless it is a page of its own
    if (index_only) toc else c('::: {.callout-tip data-legend="Index"}', '', toc, '', ':::')
  }
  toc = c(toc, '')

  r = '(<a href=")[.][.]/[.][.]/([^/]+)/help/'
  res = gsub(r, '\\1https://rdrr.io/cran/\\2/man/', res)
  res = gsub(" (id|class)='([^']+)'", ' \\1="\\2"', res)  # ' -> "
  if (is.null(topic)) {
    res = gsub('<h3>', '<h3 class="unnumbered">', res, fixed = TRUE)
  } else {
    # the page title is the Rd title (set by write_manual()), so drop the h2 and
    # raise the sections (description, usage, etc.) to h2
    title = rd_title(res)
    res = sub(rd_h2, '', res, perl = TRUE)
    res = gsub('<h3>', '<h2 class="unnumbered">', res, fixed = TRUE)
    res = gsub('</h3>', '</h2>', res, fixed = TRUE)
  }
  # add the class to tables (which may already have a class)
  res = match_replace(res, '<table\\b[^>]*>', function(x) {
    ifelse(
      grepl('\\sclass="', x), sub('(\\sclass=")', '\\1desc-table ', x),
      sub('<table', '<table class="desc-table"', x, fixed = TRUE)
    )
  })
  res = match_replace(res, '<(h[1-6])[^>]*>(?s).+?</\\1>', function(x) {
    gsub('\n', ' ', x)  # remove \n in headings so build_toc() can be correctly build TOC
  })
  res = gsub('<code style="[^"]+">', '<code>', res)
  res = gsub('<code id="[^"]+">', '<code>', res)
  res = gsub('(<code[^>]*>)\\s+', '\\1', res)
  res = gsub('\\s+(</code>)', '\\1', res)
  res = gsub('<div class="sourceCode"><pre>(.+?)</pre></div>', '<pre><code>\\1</code></pre>', res)
  res = gsub('<div class="sourceCode ([^"]+)"><pre>(.+?)</pre></div>', '<pre><code class="language-\\1">\\2</code></pre>', res)
  res = gsub('<code class="language-R"', '<code class="language-r"', res, fixed = TRUE)
  res = gsub('&#8288;', '', res, fixed = TRUE)
  res = gsub(
    '<img src="../help/figures/', paste0('<img src="', if (paged) '../', 'man/figures/'),
    res, fixed = TRUE
  )
  # the callout assets are not in the default site assets
  res = new_asis(c(toc, res, vest(if (!paged) 'callout', css = '@manual')))
  # the title (in HTML) of a single page
  if (!is.null(topic)) attr(res, 'title') = title
  res
}

# convert an Rd object to HTML, starting from the title (h2)
rd2html = function(x, links, file) {
  txt = ''
  con = textConnection('txt', 'w', local = TRUE, encoding = 'UTF-8')
  tryCatch(
    tools::Rd2HTML(x, Links = links, out = con), error = function(e) {
      warning("The Rd file '", file, "' appears to be malformed.", call. = FALSE)
      stop(e)
    }, finally = close(con))
  # extract body, which may end at </main> (R 4.4.x) or </div></body> (R 4.3.x)
  txt = gsub('(?s).*?(?=<h2)', '', one_string(txt), perl = TRUE)
  gsub('(</main>|</div>\\s*</body>).*', '', txt)
}

# the h2 heading (the Rd title) of HTML generated by rd2html()
rd_h2 = '(?s)<h2[^>]*>(.*?)</h2>\\s*'
rd_title = function(x) {
  x = sub(rd_h2, '\\1', regmatches(x, regexpr(rd_h2, x, perl = TRUE)), perl = TRUE)
  str_trim(gsub('\\s+', ' ', x))
}

# Rd pages of a package, their aliases, and groups (with filenames, instead of
# indices, as items) for pkg_manual(); `db` has the overview page (if any) first
manual_db = function(name, groups = NULL, overview = TRUE) {
  db = tools::Rd_db(name)  # all Rd pages
  intro = paste0(name, '-package.Rd')  # the name-package entry (package overview)
  entries = setdiff(names(db), intro)
  internal = vapply(entries, function(i) {
    kwd = uapply(db[[i]], function(x) if (attr(x, 'Rd_tag') == '\\keyword') x)
    'internal' %in% kwd
  }, logical(1))
  # internal pages are only kept if a group names them explicitly (like
  # pkgdown, e.g., for re-exported functions)
  if (!length(groups)) entries = entries[!internal]
  db = db[c(if (overview && intro %in% names(db)) intro, entries)]
  al = lapply(db, Rd_aliases)
  if (length(groups)) {
    keys = .mapply(c, sans_ext(entries), al[entries])
    groups = lapply(group_items(keys, groups, internal), function(g) {
      g$i = entries[g$i]
      g
    })
    # list pages by group, with the overview page (if any) first
    db = db[c(intersect(intro, names(db)), unlist(lapply(groups, `[[`, 'i')))]
    al = al[names(db)]
  }
  list(db = db, al = al, groups = groups, intro = intro)
}

# base name of the page of a help topic (an Rd filename, with or without .Rd)
manual_file = function(x) alnum_id(sub('[.]Rd$', '', x))

# a boolean field under `site` in _litedown.yml in the current directory
site_flag = function(key) isTRUE(yml_config('.')[['site']][[key]])

# individual help pages are the default only inside pkg_site() (which sets
# R_LITEDOWN_PKG), since only it can write them before rendering; a site can opt
# out via `full_manual: true`
manual_full = function() is.na(Sys.getenv('R_LITEDOWN_PKG', NA)) || site_flag('full_manual')

# Write one Markdown file per help page under `manual/` in the current (site)
# directory, which fuse_site() renders as normal pages (so they must exist
# before it starts; manual.Rmd only builds the index that links to them).
# Returns the file paths.
write_manual = function(name, examples = list(), groups = site_groups('manual')) {
  dir_create('manual')
  db = manual_db(name, groups)$db
  in_dir('manual', uapply(names(db), function(i) {
    f = paste0(manual_file(i), '.md')
    x = pkg_manual(
      name, number_sections = FALSE, examples = examples, groups = groups,
      full = FALSE, topic = i
    )
    title = gsub('(["\\\\])', '\\\\\\1', attr(x, 'title'))
    write_utf8(c('---', sprintf('title: "%s"', title), '---', '', x), f)
    file.path('manual', f)
  }))
}

#' @return `pkg_articles()` returns a list of links to the articles of the
#'   package (and their source files) in Markdown. Articles are the built
#'   vignettes (the `doc` directory of the installed package is copied to the
#'   current directory), plus the `.Rmd`/`.md` files under `vignettes/articles/`
#'   and `articles/` in the package source directory. Vignettes
#'   that use the `litedown::vignette` engine and these extra articles are
#'   rendered again as normal site pages under `articles/` by [pkg_site()].
#' @rdname pkg_desc
#' @export
pkg_articles = function(name = detect_pkg(), groups = site_groups('articles')) {
  path = attr(name, 'path') %||% system.file(package = name)
  res = tools::getVignetteInfo(name)
  arts = article_sources(path)
  if (!nrow(res) && !length(arts)) return('This package has no vignettes.')
  src = res[, 'File']
  if (nrow(res)) {
    file.copy(system.file('doc', package = name), './', recursive = TRUE)
    # redirect doc/index.html to articles.html
    if (!'index.html' %in% res[, 'PDF']) writeLines(
      "<script>location.href = location.href.replace(/\\/[^\\/]*$/, '') + '/../articles.html';</script>",
      'doc/index.html'
    )
    # clean up .R files
    file.remove(file.path('doc', setdiff(res[, 'R'], '')))
    # litedown vignettes are listed as site pages, so drop the built copies
    unlink(file.path('doc', res[src %in% basename(arts), 'PDF']))
  }
  # link the sources to GitHub if they are found in the package source directory
  link = function(f, local) {
    (if (!is_roaming()) github_file_url(path, f)) %||% local
  }
  # TODO: show excerpts of vignettes if possible
  rows = c(
    lapply(which(!src %in% basename(arts)), function(i) {
      list(res[i, 'Title'], file.path('doc', res[i, 'PDF']), src[i],
           link(file.path('vignettes', src[i]), file.path('doc', src[i])))
    }),
    lapply(arts, function(a) {
      b = basename(a)
      # use the copy under articles/ (in the site dir) if it exists
      f = if (file_exists(f2 <- file.path('articles', b))) f2 else
        relative_path(file.path(path, a))
      h = if (is_roaming()) paste0(f, '?preview=2') else with_ext(f, 'html')
      list(md_title(read_utf8(file.path(path, a)), b), h, b, link(a, f))
    })
  )
  items = vapply(rows, function(r) {
    sprintf('- [%s](%s) (source: [%s](%s))\n', r[[1]], r[[2]], r[[3]], r[[4]])
  }, '')
  if (!length(groups)) return(new_asis(items))
  keys = lapply(rows, function(r) c(sans_ext(r[[3]]), r[[3]], r[[1]]))
  new_asis(group_blocks(items, keys, groups))
}

# title from the YAML field `title` of a Markdown file, or the filename
md_title = function(x, file) {
  t = grep_sub('^title: ["\']?(.+?)["\']?\\s*$', '\\1', x)
  if (length(t)) t[1] else sans_ext(file)
}

# paths (relative to the package root `path`) of articles that are rendered as
# site pages: vignettes using the litedown engine, and all articles under
# vignettes/articles/ and articles/. The first of files with the
# same name wins.
article_sources = function(path) {
  engine = '%\\\\VignetteEngine\\{litedown::vignette\\}'
  one = function(d, all) {
    fs = list.files(file.path(path, d), '[.]R?md$')
    fs = fs[!grepl('^[_.]', fs) & tolower(sans_ext(fs)) != 'readme']
    if (!all) fs = fs[vapply(fs, function(f) {
      any(grepl(engine, read_utf8(file.path(path, d, f)), perl = TRUE))
    }, NA)]
    file.path(d, fs)
  }
  x = c(one('vignettes', FALSE), one('vignettes/articles', TRUE), one('articles', TRUE))
  x[!duplicated(basename(x))]
}

# copy article sources and their supporting files (images, data, child
# documents, etc.) to the `articles/` directory of the site
copy_articles = function(path, files, to = 'articles') {
  if (!length(files)) return()
  dir_create(to)
  for (d in unique(dirname(files))) {
    fs = list.files(file.path(path, d), all.files = TRUE, no.. = TRUE, full.names = TRUE)
    # other inputs (and outputs) are not articles, except _* files (e.g., children)
    b = basename(fs)
    skip = (grepl('[.](R?md|html|R)$', b) & !grepl('^_', b)) | b == 'articles'
    file.copy(fs[!skip], to, recursive = TRUE, overwrite = TRUE)
  }
  file.copy(file.path(path, files), to, overwrite = TRUE)
}

# a block of links in the TOC, optionally with a heading and a description; the
# heading is a normal section (h2) if the block is the content of a page
toc_block = function(x, title = NULL, desc = NULL, page = FALSE) {
  c(if (length(title)) if (page) sprintf('<h2>%s</h2>', title) else
      sprintf('<h3 class="unnumbered unlisted">%s</h3>', title),
    if (length(desc)) sprintf('<p>%s</p>', desc),
    # a page has a (Markdown) list; otherwise links in a paragraph
    if (page) c('', x, '') else c('<p>', x, '</p>'))
}

# groups from the field `site: {key}` in _litedown.yml in the current directory.
# The config is a map from group titles to either `contents` or a map with
# `desc` and `contents` (xfun::taml_file() can't parse lists of maps).
site_groups = function(key) {
  g = yml_config('.')[['site']][[key]]
  if (!length(g)) return()
  .mapply(function(title, x) {
    if (!is.list(x)) x = list(contents = x)
    c(list(title = title), x)
  }, names(g), g)
}

# put items in groups, each with a heading, an optional description, and the
# items (wrapped in a fenced Div with the attributes `wrap` if provided)
group_blocks = function(items, keys, groups, wrap = NULL) {
  unlist(lapply(group_items(keys, groups), function(g) {
    x = unlist(items[g$i])
    if (length(wrap)) x = fenced_div(x, wrap)
    c(paste0('## ', g$title, '\n'), if (length(g$desc)) c(g$desc, ''), x, '')
  }))
}

# split items into groups by matching the character vectors `keys` (one per
# item) against `contents` of each group; the first matching group wins, and
# unmatched items go to the last group `Other`. Contents that match no item
# (e.g., stale names) are an error. Returns a list of groups, each
# with `title`, `desc`, and the indices `i` of items (empty groups dropped).
# Items flagged in `skip` are only included if a group matches them.
group_items = function(keys, groups, skip = FALSE) {
  id = rep(NA_integer_, length(keys)); bad = NULL
  for (j in seq_along(groups)) {
    hit = lapply(as.character(unlist(groups[[j]]$contents)), function(p) {
      h = vapply(keys, function(k) any(grepl(utils::glob2rx(p), k)), NA)
      if (!any(h)) bad <<- c(bad, sprintf('"%s" (group "%s")', p, groups[[j]]$title))
      h
    })
    id[is.na(id) & Reduce(`|`, hit, FALSE)] = j
  }
  if (length(bad)) stop(
    'No match for the following items in the groups: ', paste(bad, collapse = ', '),
    call. = FALSE
  )
  groups = c(groups, list(list(title = 'Other')))
  id[is.na(id) & !skip] = length(groups)
  res = lapply(seq_along(groups), function(j) {
    g = groups[[j]]
    list(title = g$title, desc = g$desc, i = which(id == j))
  })
  res[lengths(lapply(res, `[[`, 'i')) > 0]
}

run_examples = function(html, config, path) {
  config$dev.path = path = paste0(config$dev.path, path)
  on.exit(del_empty_dir(dirname(path)), add = TRUE)
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
          if (is_blank(b)) b = character()
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

#' Build a website for an R package
#'
#' Generate a website from a template shipped with \pkg{litedown}, which
#' contains pages for the package description, news, manual, vignettes,
#' examples, and source code.
#'
#' Template files are copied from `system.file('site', package = 'litedown')` to
#' the `output` directory. Files that already exist under `output` are not
#' overwritten, i.e., you can customize any page by providing your own version
#' of it. Pages that do not apply to the package are deleted, e.g., `news.Rmd`
#' is deleted if the package has neither `NEWS.md` nor `inst/NEWS.Rd`.
#'
#' The source files copied from the template (e.g., `index.Rmd` and
#' `_litedown.yml`) are removed after the site is built, so only your own files
#' and the output remain in `output`.
#'
#' Template pages that you do not want can be listed in the field `exclude`
#' under `site` in `_litedown.yml` under `output`, e.g.,
#' `exclude: [code.Rmd, news.Rmd]`. They will not be copied. By default, only
#' `code.Rmd` is excluded, and `exclude: NA` means to exclude nothing.
#'
#' If the package has a logo (e.g., a hex sticker) at `man/figures/logo.png`
#' (or `.svg`, `.jpg`, etc.), it will be shown at the top right of the home page.
#' A file `logo.png` (or other formats) under `output` takes precedence.
#'
#' If the package contains a book project (i.e., `book/_litedown.yml` exists),
#' the book will be built via [fuse_book()] and placed under the `book/`
#' subdirectory of the site.
#'
#' The package must have been installed, because rendering the site requires the
#' installed version of it (e.g., [pkg_manual()] reads its Rd database, and
#' `articles.Rmd` lists its built vignettes). The pages generated from the
#' package source (e.g., `examples.Rmd` and the book under `book/`) require the
#' package root directory, which is taken from the `path` attribute of `name`.
#' @inheritParams pkg_desc
#' @param output The directory in which the site is built (created if it does not
#'   exist). It holds both the site source (i.e., the template) and the output
#'   files.
#' @return Output file paths (invisibly).
#' @seealso [fuse_site()], which renders the site after the template has been
#'   set up.
#' @export
#' @examples
#' \dontrun{
#' # run this in the root directory of a package
#' litedown::pkg_site()
#' }
pkg_site = function(name = detect_pkg(), output = 'site') {
  if (!loadable(name)) stop(
    "The package '", name, "' needs to be installed before its site can be built."
  )
  path = normalize_path(attr(name, 'path') %||% system.file(package = name))
  # tell detect_pkg() where the package is (the site dir may live anywhere)
  vars = set_envvar(c(
    R_LITEDOWN_PKG = path, R_LITEDOWN_GITHUB = github_link(path) %||% ''
  ))
  on.exit(set_envvar(vars), add = TRUE)
  # the footer is filled in below; restore it if it is the user's own (a copy
  # from the template is removed by site_cleanup())
  footer = file.path(output, '_footer.md')
  if (file_exists(footer)) {
    old = read_utf8(footer)
    on.exit(write_utf8(old, footer), add = TRUE)
  }
  copied = site_skeleton(output, path)
  on.exit(site_cleanup(output, copied), add = TRUE)
  site_footer(footer, path)
  res = in_dir(output, {
    # create a placeholder so that the book is included in the nav menu
    cfg = file.path(path, 'book/_litedown.yml')
    if (book <- file_exists(cfg)) {
      dir_create('book'); file.create('book/index.html')
    }
    # per-topic help pages must exist before fuse_site() collects the inputs
    pages = if (!manual_full()) write_manual(name)
    out = fuse_site('.')
    unlink(pages)
    if (book) {
      # run from the package root so that `$input$` is relative to it; use a new
      # R session so that book code cannot clobber our variables
      in_dir(path, Rscript_call(function() {
        Sys.unsetenv('R_LITEDOWN_PKG')
        litedown::fuse_book('book')
      }))
      copy_dir(dirname(cfg), 'book')
      unlink(c('book/_litedown.yml', list.files('book', '[.]Rmd$', full.names = TRUE)))
    }
    dir_create('playground')
    copy_dir(file.path(path, 'playground'), 'playground')
    if (dir.exists('examples')) write_utf8(
      grep('^[^_].+[.](Rmd|R)$', list.files('examples'), value = TRUE),
      'playground/examples.txt'
    )
    out
  })
  invisible(res)
}

# README.md of a package prepared for the home page of its site (NULL if none):
# badges go to the right sidebar, and lines before them and the title are removed
pkg_readme = function(name = detect_pkg()) {
  f = file.path(attr(name, 'path'), 'README.md')
  if (!file_exists(f)) return()
  x = read_utf8(f)
  i = which(x == '<!-- badges: start -->')
  if (length(i)) {
    j = which(x == '<!-- badges: end -->')
    # put badges in the right sidebar
    if (length(j <- j[j > i[1]])) x = append(append(
      x, c('', ':::', ''), j[1]
    ), c('::: {.side .side-right}', ''), i[1] - 1)
    # remove lines before badges since they usually contain duplicated info (e.g., title)
    x = x[seq_along(x) >= i[1]]
  }
  i = which(tolower(x) == paste('#', tolower(name)))
  # remove the title line if it still exists
  if (length(i)) x = x[-i[1]]
  x
}

# Markdown of the package logo (e.g., a hex sticker) with the class `logo`: the
# logo file in the current (site) dir, or man/figures/ of the package; NULL if
# there is none or the (prepared) README already shows it. The home page copies
# man/figures to its own dir.
pkg_logo = function(name = detect_pkg(), readme = pkg_readme(name)) {
  dirs = c('.', file.path(attr(name, 'path'), 'man/figures'))
  for (i in 1:2) if (length(x <- list.files(dirs[i], logo_regex))) break
  if (!length(x) || any(grepl(x[1], readme, fixed = TRUE))) return()
  # styled by .logo in site.css
  sprintf('![%s logo](%s%s){.logo}', name, c('', 'man/figures/')[i], x[1])
}

logo_regex = '^logo[.](png|jpe?g|gif|svg|webp)$'

# fill in the pre-computed variables in the footer file (included by
# _litedown.yml); $edit$ is left for set_site_options() since it differs from
# page to page.
site_footer = function(f, path) {
  if (!file_exists(f)) return()
  d = read_desc(structure('', path = path), c('Authors@R', 'Author'))
  names(d) = c('Authors@R', 'Author')
  vars = list(
    authors = xfun::join_words(pkg_authors(d, 'aut', FALSE)),
    version = as.character(packageVersion('litedown'))
  )
  x = read_utf8(f)
  for (i in names(vars)) x = gsub(sprintf('$%s$', i), vars[[i]], x, fixed = TRUE)
  write_utf8(x, f)
}

# pages of the template that should not be copied: `site: exclude` in the
# _litedown.yml under `dir` ('code.Rmd' by default, and NA means none)
site_exclude = function(dir) {
  x = yml_config(dir)[['site']][['exclude']] %||% 'code.Rmd'
  x[!is.na(x)]
}

# copy the site template to the site dir and delete pages that do not apply;
# returns the template files that were copied
site_skeleton = function(output, path) {
  dir_create(output)
  exclude = site_exclude(output)
  # copy template files, but keep files that already exist in the site dir
  tpl = pkg_file('site')
  copied = NULL
  for (f in list.files(tpl, all.files = TRUE, recursive = TRUE)) {
    if (any(f == exclude | startsWith(f, paste0(exclude, '/')))) next
    if (file_exists(f2 <- file.path(output, f))) next
    dir_create(dirname(f2))
    if (file.copy(file.path(tpl, f), f2)) copied = c(copied, f)
  }
  in_dir(output, {
    if (!any(dir.exists(file.path(path, c('vignettes', 'articles')))))
      unlink('articles.Rmd')
    copy_articles(path, article_sources(path))
    if (!any(file_exists(file.path(path, c('NEWS.md', 'inst/NEWS.Rd')))))
      unlink('news.Rmd')
    if (dir.exists(d <- file.path(path, 'examples'))) {
      file.copy(d, '.', recursive = TRUE)
    } else unlink('examples.Rmd')
  })
  invisible(copied)
}

# remove the source files (.Rmd, .yml, and _* in the top directory) that were
# copied from the template, since they are not part of the site; the files in
# subdirectories (e.g., the playground) are assets of the site
site_cleanup = function(dir, files) {
  files = files[!grepl('/', files) & grepl('^_|[.](Rmd|yml)$', files)]
  unlink(file.path(dir, files))
}

# copy the content (instead of the directory itself) of a dir to another dir
copy_dir = function(from, to) {
  if (!dir.exists(from)) return()
  fs = list.files(from, all.files = TRUE, no.. = TRUE, full.names = TRUE)
  file.copy(fs, to, recursive = TRUE, overwrite = TRUE)
}

# read package metadata: from the DESCRIPTION file if the package root is found,
# otherwise from the installed package
read_desc = function(name = detect_pkg(), fields = NULL) {
  if (is.character(p <- attr(name, 'path'))) {
    d = read.dcf(file.path(p, 'DESCRIPTION'))[1, ]
    as.list(if (is.null(fields)) d else d[fields])
  } else {
    packageDescription(name, fields = fields)
  }
}

# detect package name and root path from current and upper dirs
detect_pkg = local({
  res = NULL  # cache the detection
  function(...) {
    if (is.null(res) || !same_path('.', attr(res, 'wd'))) res <<- .detect_pkg(...)
    res
  }
})

.detect_pkg = function(error = TRUE) {
  # the root dir can be specified explicitly via an env var (e.g., by pkg_site())
  if (!is.na(p <- Sys.getenv('R_LITEDOWN_PKG', NA))) {
    desc = read_utf8(file.path(p, 'DESCRIPTION'))
    name = grep_sub('^Package: (.+?)\\s*$', '\\1', desc)[1]
    return(structure(name, path = p, wd = getwd()))
  }
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
  } else if (length(name) == 0) {
    desc = read_utf8(file.path(root, 'DESCRIPTION'))
    name = grep_sub('^Package: (.+?)\\s*$', '\\1', desc)[1]
  }
  structure(name, path = root, wd = getwd())
}

detect_news = function(name) {
  if (isTRUE(file_exists(path <- file.path(attr(name, 'path'), 'NEWS.md'))))
    path else system.file('NEWS.md', package = name)
}

# get \alias{} names in an Rd object
Rd_aliases = function(x) {
  uapply(x, function(x) if (attr(x, 'Rd_tag') == '\\alias') as.character(x))
}
