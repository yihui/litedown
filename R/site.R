#' Fuse R Markdown documents individually under a directory
#'
#' Run [fuse()] on R Markdown documents individually to generate a website.
#'
#' If a directory contains a config file `_litedown.yml`, which has a YAML field
#' `site`, the directory will be recognized as a site root directory. The YAML
#' field `output` will be applied to all R Markdown files (an individual R
#' Markdown file can provide its own `output` field in YAML to override the
#' global config). For example:
#'
#' ```yaml
#' ---
#' site:
#'   rebuild: "outdated"
#'   pattern: "[.]R?md$"
#' output:
#'   html:
#'     meta:
#'       css: ["@default"]
#'       include_before: "[Home](/) [About](/about.html)"
#'       include_after: "&copy; 2024 | [Edit]($input$)"
#' ---
#' ```
#'
#' The option `rebuild` determines whether to rebuild `.Rmd` files. Possible
#' values are:
#'
#' - `newfile`: Build an input file if it does not have a `.html` output file.
#'
#' - `outdated`: Rebuild an input file if the modification time of its `.html`
#' output file is older than the input or the config file `_litedown.yml`.
#' @param input The root directory of the site, or a vector of input file paths.
#' @return Output file paths (invisibly).
#' @examples
#' # create a minimal book example in the temp dir
#' d = tempfile()
#' litedown:::proj_skeleton(d, "site")
#' fuse_site(d)
#' if (interactive()) browseURL(file.path(d, "index.html"))
#' # clean up
#' if (xfun::is_R_CMD_check()) unlink(d, recursive = TRUE)
#' @export
fuse_site = function(input = '.') {
  info = NULL; preview = FALSE
  inputs = if (length(input) == 1 && dir.exists(input)) {
    info = proj_info('', input)
    find_input(input, TRUE, info$yaml[['site']][['pattern']])
  } else {
    info = proj_info(input[1])
    preview = is_roaming() && length(input) == 1
    input
  }
  root = info$root
  output = with_ext(inputs, '.html')
  cfg = merge_list(list(rebuild = 'outdated'), info$yaml[['site']])
  b = cfg[['rebuild']]
  if (b == 'outdated') b = 0
  # a config change affects all pages
  deps = if (!is.na(root)) file.path(root, '_litedown.yml')
  i = if (is.numeric(b)) filter_outdated(inputs, output, b, deps) else {
    if (b == 'newfile') !file_exists(output) else TRUE
  }
  opts = yaml_field(info$yaml, 'html', c('meta', 'options'))
  opts[['meta']] = merge_list(list(
    css2 = c(site_css, '@site'), js2 = c(site_js, '@nav-highlight'),
    include_before = nav_menu(info), include_after = format(Sys.Date(), '&copy; %Y')
  ), opts[['meta']])
  opts[['options']] = merge_list(
    list(embed_resources = FALSE, toc = TRUE), opts[['options']]
  )
  standalone = cfg[['standalone']]
  out = lapply(inputs[i], function(x) {
    alone = length(standalone) && any(vapply(
      standalone, function(p) grepl(glob2rx(p), basename(x)), logical(1)
    ))
    res = if (grepl('[.]md$', x)) {
      if (!alone) {
        opts = set_site_options(opts, x, root, giscus = cfg[['giscus']])
        on.exit(options(opts))
      }
      mark(x, full_output)
    } else {
      Rscript_call(
        function(x, opts, set, root, flag, output, giscus) {
          if (!is.null(opts)) set(
            opts, x, root, list(litedown.roaming = flag), giscus
          )
          litedown::fuse(x, output, envir = globalenv())
        },
        list(x, if (!alone) opts, set_site_options,
          root, is_roaming(), full_output, cfg[['giscus']]),
        fail = paste('Failed to run litedown::fuse() on', x)
      )
    }
    # resolve / to relative paths
    if (!is.na(info$root)) {
      up = relative_path(info$root, dirname(x))
      if (up == '.') up = ''
      res = match_replace(res, ' (href|src)(=")/', function(z) {
        gsub('/$', up, z)
      })
    }
    if (preview) res else write_utf8(res, with_ext(x, '.html'))
  })
  if (preview) {
    if (i) out[[1]] else xfun::file_string(output)
  } else invisible(output)
}

# common css/js for sites/books
site_css = c('@default', '@article', '@copy-button', '@heading-anchor', '@pages')
site_js = c('@sidenotes', '@toc-highlight', '@copy-button', '@heading-anchor', '@pages')

# set global options litedown.html.[meta|options] read from _litedown.yml
set_site_options = function(opts, input, root, extra = NULL, giscus = NULL) {
  m = opts[['meta']]
  edit = edit_page_url(input, root) %||% ''
  for (i in c('include_before', 'include_after')) {
    if (!is.character(m[[i]])) next
    nav = i == 'include_before'
    tag = if (nav) 'nav' else 'footer'
    x = mark(I(one_string(m[[i]], test = c(dirname(input), root))))
    # the class lets CSS tell the site menu from other <nav>s such as the TOC
    x = sprintf('<%s%s>%s</%s>', tag, if (nav) ' class="site-nav"' else '', x, tag)
    m[[i]] = sub_vars(x, list(input = I(input), edit = I(edit)))
    # drop the edit link if the page has no source file on GitHub
    if (edit == '') m[[i]] = gsub(
      '(<p>)?<a [^>]*class="edit-page"[^>]*>.*?</a>(?(1)</p>\n?)', '', m[[i]],
      perl = TRUE
    )
    # comments go before the footer, not inside it
    if (!nav) m[[i]] = paste0(giscus_html(giscus), m[[i]])
  }
  # root-relative css/js (e.g., /style.css) become relative to the page
  if (!is.na(root)) {
    up = relative_path(root, dirname(input))
    if (up == '.') up = ''
    for (i in c('css', 'js', 'css2', 'js2')) if (is.character(m[[i]])) {
      m[[i]] = sub('^/(?!/)', up, m[[i]], perl = TRUE)
    }
    if (!is.null(fl <- function_links(root, up))) {
      m[['header_includes']] = c(m[['header_includes']], fl$meta)
      m[['js2']] = c(m[['js2']], fl$js)
    }
  }
  opts[['meta']] = m
  options(c(set_names(opts, paste0('litedown.html.', names(opts))), extra))
}

# The <meta> (where the function-to-page map is, relative to a page that is
# `up` away from the site root) and the script (link-functions.js, with a copy
# in the site dir taking precedence over the CDN one) that link function calls
# in code to their help pages; NULL if the site has no such map.
function_links = function(root, up) {
  if (!file_exists(file.path(root, function_index))) return()
  list(
    meta = sprintf('<meta name="function-index" content="%s%s">', up, function_index),
    js = if (file_exists(file.path(root, 'link-functions.js')))
      paste0(up, 'link-functions.js') else '@link-functions'
  )
}

# giscus settings that differ from giscus's own defaults (documented in the book)
giscus_defaults = list(
  strict = 1, theme = 'preferred_color_scheme', loading = 'lazy'
)

# giscus comment section from `site: giscus` in _litedown.yml
giscus_html = function(cfg) {
  if (!is.list(cfg) || !length(cfg)) return()
  cfg = merge_list(giscus_defaults, cfg)
  attrs = sprintf(
    ' data-%s="%s"', names(dash_names(cfg)), html_escape(unlist(cfg), TRUE)
  )
  sprintf(paste0(
    '<section class="comments">\n',
    '<script src="https://giscus.app/client.js"%s crossorigin="anonymous" ',
    'async></script>\n</section>\n'
  ), paste(attrs, collapse = ''))
}

# GitHub URL for editing a page, if the page source also exists (at the same
# relative path) in the package of the site, e.g., an example under `examples/`
edit_page_url = function(input, root) {
  if (is.na(root) || is.null(pkg <- in_dir(root, detect_pkg(error = FALSE))))
    return()
  f = relative_path(input, root)
  # articles/ in the site may come from vignettes/, vignettes/articles/, etc.
  if (dirname(f) == 'articles') f = Find(
    function(a) basename(a) == basename(f), article_sources(attr(pkg, 'path')),
    nomatch = f
  )
  github_file_url(attr(pkg, 'path'), f, TRUE)
}

# `deps` are extra files that make all outputs outdated when they are modified
filter_outdated = function(x, x2, n, deps = NULL) {
  m1 = file.mtime(x); m2 = file.mtime(x2)
  if (length(deps)) m1 = pmax(m1, max(file.mtime(deps)), na.rm = TRUE)
  is.na(m2) | m1 - m2 > n
}

# build a nav menu from filenames under root directory; the fields `nav` and
# `nav_hide` under `site` customize the order, text, and visibility of items
nav_menu = function(info) {
  if (is.na(info$root)) return('[Home](/index.html)')
  root = sub('/+$', '', info$root)  # strip any trailing slash for reliable dirname() comparison
  site = info$yaml[['site']]
  # list all pages, since `inputs` may be a single file when previewing
  b = basename(find_input(root, FALSE, site[['pattern']]))
  # add subdirs that already have an index.html (same exclusion as find_input),
  # except `doc/` whose index.html is the vignette index of an R package
  sub_dirs = list.dirs(root, recursive = FALSE)
  sub_dirs = sub_dirs[file.exists(file.path(sub_dirs, 'index.html'))]
  dnames = setdiff(basename(sub_dirs), 'doc')
  key = c(sans_ext(b), dnames)
  urls = c(
    if (is_roaming()) paste0(b, '?preview=2') else with_ext(b, '.html'),
    paste0(dnames, '/')
  )
  nav_link = function(text, i) sprintf('[%s](/%s)', text, urls[i])
  text = menu_name(c(ifelse(is_index(b), 'home', sans_ext(b)), dnames))
  links = nav_link(text, seq_along(key))
  # the package name and version replace the Home link of a package site
  if (!is.null(brand <- nav_brand(root))) {
    i = which(key == 'index')
    links[i] = sprintf(brand, paste0('/', urls[i]))
  }
  links = nav_arrange(links, key, nav_link, site[['nav']], site[['nav_hide']])
  c(links, nav_icons(root))
}

# reorder menu items by the names in `nav` (unlisted items follow in their
# default order), replace the text of an item if `nav` gives one, and drop items
# matching the patterns in `hide`
nav_arrange = function(links, key, link, nav, hide) {
  if (!length(nav) && !length(hide)) return(links)
  # a plain list of names only sets the order
  nms = if (is.null(names(nav))) as.character(unlist(nav)) else names(nav)
  text = if (is.null(names(nav))) rep('', length(nms)) else
    vapply(nav, function(x) paste(x, collapse = ''), '')
  pats = as.character(unlist(hide))
  hit = lapply(pats, function(p) grepl(glob2rx(p), key))
  bad = c(setdiff(nms, key), pats[lengths(lapply(hit, which)) == 0])
  if (length(bad)) stop(
    'No match for the following items in the nav menu: ',
    paste0('"', bad, '"', collapse = ', '), call. = FALSE
  )
  hidden = Reduce(`|`, hit, rep(FALSE, length(key)))
  if (any(i <- nms %in% key[hidden])) stop(
    'Items cannot be both in `nav` and `nav_hide`: ',
    paste0('"', nms[i], '"', collapse = ', '), call. = FALSE
  )
  i = match(nms, key)
  j = text != ''
  links[i[j]] = link(text[j], i[j])
  i = c(i, setdiff(seq_along(key), i))
  links[i[!hidden[i]]]
}

# link of the package name and version (`%s` is the URL), NULL if not a package
nav_brand = function(root = '.') {
  if (is.null(pkg <- in_dir(root, detect_pkg(error = FALSE)))) return()
  v = read_desc(pkg, 'Version')[[1]]
  sprintf('[%s](%%s){.brand} <small class="version">v%s</small>', pkg, v)
}

# icons at the right end of the menu (see site.css, where the empty span pushes
# them to the right): the print button of pages.js, an edit link (`$edit$` is set
# per page), and a GitHub icon if the site `root` belongs to a package with a repo
nav_icons = function(root = '.') {
  c(
    '<span class="pagesjs-toggle"></span>',
    '[![Edit this page](https://cdn.jsdelivr.net/npm/@primer/octicons@19/build/svg/pencil-24.svg){.no-invert}]($edit$ "Edit this page"){.edit-page}',
    nav_github(root)
  )
}

nav_github = function(root = '.') {
  if (is.null(pkg <- in_dir(root, detect_pkg(error = FALSE)))) return()
  if (!length(repo <- github_link(attr(pkg, 'path')))) return()
  sprintf(paste0(
    '[![GitHub](https://cdn.jsdelivr.net/npm/@primer/octicons@19/build/svg/',
    'mark-github-24.svg){.no-invert}](%s "GitHub repository")'
  ), repo)
}

menu_name = function(x) {
  tools::toTitleCase(gsub('[-_]', ' ', x))
}

#' Fuse multiple R Markdown documents to a single output file
#'
#' This is a helper function to [fuse()] `.Rmd` files and convert all their
#' Markdown output to a single output file, which is similar to
#' `bookdown::render_book()`, but one major differences is that all HTML output
#' is written to one file, instead of one HTML file per chapter.
#'
#' If the output format needs to be customized, the settings should be written
#' in the config file `_litedown.yml`, e.g.,
#'
#' ```yaml
#' ---
#' output:
#'   html:
#'     options:
#'       toc:
#'         depth: 4
#'   latex:
#'     meta:
#'       documentclass: "book"
#' ```
#'
#' In addition, you can configure the book via the `book` field, e.g.,
#'
#' ```yaml
#' ---
#' book:
#'   new_session: true
#'   subdir: false
#'   pattern: "[.]R?md$"
#'   chapter_before: "Information before a chapter."
#'   chapter_after: "This chapter was generated from `$input$`."
#' ---
#' ```
#'
#' The option `new_session` specifies whether to render each input file in the
#' current R session or a separate new R session; `chapter_before` and
#' `chapter_after` specify text to be added to the beginning and end of each
#' file, respectively, which accepts some variables (e.g., `$input$` is the
#' current input file path).
#' @inheritParams fuse
#' @param input A directory or a vector of file paths. By default, all
#'   `.Rmd`/`.md` files under the current working directory are used as the
#'   input, except for filenames that start with `.` or `_` (e.g., `_foo.Rmd`),
#'   or `.md` files with the same base names as `.Rmd` files (e.g., `bar.md`
#'   will not be used if `bar.Rmd` exists). For a directory `input`, the file
#'   search will be recursive if `input` ends with a slash (i.e.,
#'   sub-directories will also be searched). If a file named `index.Rmd` or
#'   `index.md` exists, it will always be treated as the first input file. Input
#'   files can also be specified in the config file `_litedown.yml` (in the
#'   `input` field under `book`).
#' @return An output file path or the output content, depending on the `output`
#'   argument.
#' @examples
#' # create a minimal book example in the temp dir
#' d = tempfile()
#' litedown:::proj_skeleton(d, "book")
#' fuse_book(d)
#' if (interactive()) browseURL(file.path(d, "index.html"))
#' # clean up
#' if (xfun::is_R_CMD_check()) unlink(d, recursive = TRUE)
#' @export
fuse_book = function(input = '.', output = NULL, envir = parent.frame()) {
  # when input is c(dir, file1, file2, ...), we find book files under dir, but
  # only preview file1, file2, ...
  if (dir.exists(input[1])) {
    preview = input[-1]; input = input[1]
  } else preview = NULL

  yaml = NULL
  # search for book files or read from config if input is a dir
  if (length(input) == 1 && dir.exists(input)) {
    yaml = yml_config(input)
    cfg = yaml[['book']]
    input = file.path(input, cfg[['input']]) %|%
      find_input(input, cfg[['subdir']] %||% grepl('/$', input), cfg[['pattern']])
  } else {
    # if input files are provided directly, read config from the dir of first file
    cfg = if (length(input)) {
      yaml = yml_config(dirname(input[1]))
      yaml[['book']]
    }
  }
  if (length(input) == 0) stop('No input was provided or found.')
  input = sub('^[.]/+', '', input)  # clean up the leading ./ in paths

  full = is_output_full(output)
  format = detect_format(output, yaml)
  output = auto_output(input[1], output, format)
  cfg = merge_list(list(
    new_session = FALSE, chapter_before = '', chapter_after = "Source: `$input$`"
  ), cfg)

  # provide a simpler way to configure timing in YAML; only env vars are
  # inherited in new R sessions, so we attach the timing path to R_LITEDOWN_TIME
  if (is.character(p <- cfg$time)) {
    # treat relative path as a path relative to the first input's cache dir
    if (is_rel_path(p))
      p = file.path(paste0(sans_ext(normalize_path(input[1])), '__cache'), p)
    vars = set_envvar(c(R_LITEDOWN_TIME = p))
    on.exit(set_envvar(vars), add = TRUE)
    if (file_exists(p)) {
      # filter out data from input files that do not belong to the book
      d = readRDS(p)
      if (!all(i <- d$source %in% input)) {
        d = d[i, ]; saveRDS(d, p)
      }
    } else dir_create(dirname(p))
  }

  res = lapply(preview %|% input, function(x) {
    out = if (grepl('[.]md$', x)) read_utf8(x) else {
      fmt = paste0('markdown:', format)  # generate intermediate markdown output
      if (cfg$new_session) {
        Rscript_call(fuse, list(x, fmt))
      } else {
        fuse(x, fmt, NULL, envir)
      }
    }
    # remove YAML in the preview mode or for non-index chapters since we only need the body
    if (length(preview) || isFALSE(.env$current_index) || x != input[1])
      out = sans_yaml(out)

    if (format != 'html') return(out)
    # add input filenames to the end for HTML output and wrap each file in a div
    info = function(cls) c(
      sprintf('::: {.chapter-%s .side .side-right}', cls),
      sub_vars(cfg[[sprintf('chapter_%s', cls)]], list(input = I(x))), ':::'
    )
    # for the first input, the fenced Divs should be inserted after YAML
    h = if (length(preview) == 0 && x == input[1]) {
      out = split_lines(out)
      if (length(i <- xfun:::locate_yaml(out)) >= 2) {
        i = seq_len(i[2]); h = out[i]; out = out[-i]
        h
      }
    }
    c(
      h, sprintf('<article class="chapter" data-source="%s">', x), '',
      info('before'), '', out, '', info('after'), '', '</article>'
    )
  })
  tweak_options(format, yaml, list(
    body_tag = 'div', css2 = c(site_css, '@book'), js2 = site_js
  ), toc = length(preview) == 0)
  fuse_output(input[1], output, unlist(res), full)
}

# read the config file _litedown.yml
yml_config = function(d) {
  if (file_exists(cfg <- file.path(d, '_litedown.yml'))) {
    yaml = xfun::taml_file(cfg)
    if (!is.null(yaml2 <- normalize_yaml(yaml))) yaml = yaml2
    yaml
  }
}

proj_skeleton = function(path, type = 'book') {
  dir_create(path)
  file.copy(list.files(pkg_file('rstudio', type), full.names = TRUE), path)
  invisible(path)
}

site_pattern = '[.][Rq]?md$'

# find input files under a directory
find_input = function(d, deep = grepl('/$', d), pattern = NULL) {
  if (!is.character(pattern)) pattern = site_pattern
  x = list.files(d, pattern, full.names = TRUE, recursive = deep)
  # exclude .* and _* files/dirs (test paths relative to d, so ancestor dirs
  # like GHA's `_temp` don't accidentally match)
  rel = if (deep) substr(x, nchar(d) + 2, nchar(x)) else basename(x)
  x = x[!grepl('(^|/)[_.]', rel)]
  # exclude readme
  x = x[tolower(basename(sans_ext(x))) != 'readme']
  # for .md files, don't include them if they have .Rmd/.qmd files
  b = sans_ext(x); i = file_ext(x) == 'md'
  x = x[!i | !(b %in% sub('[.][Rq]md$', '', x))]
  x = reorder_input(x)
  x = sub('^[.]/+', '', x)
  x
}

# move index.[Rq]md to the first
reorder_input = function(x) {
  i = is_index(x)
  index = x[i][which.min(nchar(x[i]))]  # take the shortest path
  c(index, setdiff(x, index))
}

is_index = function(x) sans_ext(basename(x)) == 'index'

# temporarily set global metadata and options (inheriting from index.Rmd)
tweak_options = function(format, yaml, meta = NULL, toc = TRUE, options = NULL) {
  nms = paste0('litedown.', format, c('.meta', '.options'))
  defaults = list(
    merge_list(
      .Options[[nms[1]]], meta, yaml_field(yaml, format, 'meta')
    ),
    merge_list(
      .Options[[nms[2]]], options,
      list(toc = toc, number_sections = TRUE, embed_resources = FALSE),
      yaml_field(yaml, format, 'options')
    )
  )
  names(defaults) = nms
  opts = options(defaults)
  exit_call(function() options(opts))
}
