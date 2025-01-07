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
#' output file is newer than the input.
#' @param input The root directory of the site, or a vector of input file paths.
#' @return Output file paths (invisibly).
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
  i = if (is.numeric(b)) filter_outdated(inputs, output, b) else {
    if (b == 'newfile') !file_exists(output) else TRUE
  }
  opts = yaml_field(info$yaml, 'html', c('meta', 'options'))
  opts[['meta']] = merge_list(list(
    css = c("@default", "@article", '@site', "@copy-button", "@heading-anchor"),
    js = c("@sidenotes", "@appendix", "@toc-highlight", "@copy-button", "@heading-anchor"),
    include_before = nav_menu(info), include_after = format(Sys.Date(), '&copy; %Y')
  ), opts[['meta']])
  opts[['options']] = merge_list(
    list(embed_resources = FALSE, toc = TRUE), opts[['options']]
  )
  out = lapply(inputs[i], function(x) {
    res = if (grepl('[.]md$', x)) {
      opts = set_site_options(opts, x, root); on.exit(options(opts))
      mark(x, full_output)
    } else {
      Rscript_call(
        function(x, opts, set, root, flag, output) {
          set(opts, x, root, list(litedown.roaming = flag))
          litedown::fuse(x, output, envir = globalenv())
        },
        list(x, opts, set_site_options, root, is_roaming(), full_output),
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

# set global options litedown.html.[meta|options] read from _litedown.yml
set_site_options = function(opts, input, root, extra = NULL) {
  m = opts[['meta']]
  for (i in c('include_before', 'include_after')) {
    if (!is.character(m[[i]])) next
    tag = if (i == 'include_before') 'nav' else 'footer'
    x = mark(I(one_string(m[[i]], test = c(dirname(input), root))))
    x = sprintf('<%s>%s</%s>', tag, x, tag)
    m[[i]] = sub_vars(x, list(input = I(input)))
  }
  opts[['meta']] = m
  options(c(set_names(opts, paste0('litedown.html.', names(opts))), extra))
}

filter_outdated = function(x, x2, n) {
  m1 = file.mtime(x); m2 = file.mtime(x2); is.na(m2) | m1 - m2 > n
}

# build a nav menu from filenames under root directory
nav_menu = function(info) {
  if (is.na(info$root)) return('[Home](/index.html)')
  files = find_input(info$root, FALSE, info$yaml[['site']][['pattern']])
  b = basename(files)
  x = gsub('[-_]', ' ', sans_ext(ifelse(is_index(b), 'home', b)))
  sprintf(
    '[%s](/%s)', tools::toTitleCase(x),
    if (is_roaming()) paste0(b, '?preview=2') else with_ext(b, '.html')
  )
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
    # remove YAML in the preview mode since we only need the body
    if (length(preview)) out = yaml_body(split_lines(out), parse = FALSE)$body

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
      h, sprintf('::: {.chapter .body data-source="%s"}', x),
      info('before'), '', out, '', info('after'), ':::'
    )
  })
  tweak_options(format, yaml, list(
    body_class = '',
    css = c("@default", "@article", "@book", "@copy-button", "@heading-anchor"),
    js = c("@sidenotes", "@appendix", "@toc-highlight", "@copy-button", "@heading-anchor")
  ), toc = length(preview) == 0)
  fuse_output(input[1], output, unlist(res), full)
}

# read the config file _litedown.yml
yml_config = function(d) {
  if (file_exists(cfg <- file.path(d, '_litedown.yml'))) xfun::taml_file(cfg)
}

site_pattern = '[.][Rq]?md$'

# find input files under a directory
find_input = function(d, deep = grepl('/$', d), pattern = NULL) {
  if (!is.character(pattern)) pattern = site_pattern
  x = list.files(d, pattern, full.names = TRUE, recursive = deep)
  # exclude .* and _* files
  x = x[!grepl('^[_.]', basename(x))]
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
