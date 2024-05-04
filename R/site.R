#' Fuse multiple R Markdown documents to a single output file
#'
#' This is a helper function to [fuse()] `.Rmd` files and convert all their
#' Markdown output to a single output file, which is similar to
#' `bookdown::render_book()`, but one major differences is that all HTML output
#' is written to one file, instead of one HTML file per chapter.
#'
#' If the output format ([html_format()] or [latex_format()]) needs to be
#' customized, the configurations should be set in the YAML metadata of the
#' first input file (typically `index.Rmd`), e.g.,
#'
#' ```
#' ---
#' output:
#'   litedown::html_format:
#'     meta:
#'       css: ["@default", "custom.css"]
#'     options:
#'       number_sections: false
#'   litedown::latex_format:
#'     meta:
#'       documentclass: "book"
#' ```
#'
#' In addition, you can configure the book via the `book` field under the
#' top-level YAML field `litedown`, e.g.,
#'
#' ```
#' ---
#' litedown:
#'   book:
#'     new_session: true
#'     chapter_begin: "Information before a chapter."
#'     chapter_end: "This chapter was generated from `$input$`."
#' ---
#' ```
#'
#' The option `new_session` specifies whether to render each input file in the
#' current R session or a separate new R session; `chapter_begin` and
#' `chapter_end` specify text to be added to the beginning and end of each file,
#' respectively, which accepts some variables (e.g., `$input$` is the current
#' input file path).
#' @inheritParams fuse
#' @param input A directory or a vector of file paths. By default, all
#'   `.Rmd`/`.md` files under the current working directory are used as the
#'   input, except for filenames that start with `.` or `_` (e.g., `_foo.Rmd`),
#'   or `.md` files with the same base names as `.Rmd` files (e.g., `bar.md`
#'   will not be used if `bar.Rmd` exists). For a directory `input`, the file
#'   search will be recursive if `input` ends with a slash (i.e.,
#'   sub-directories will also be searched). If a file named `index.Rmd` or
#'   `index.md` exists, it will always be treated as the first input file.
#' @return An output file path or the output content, depending on the `output`
#'   argument.
#' @export
fuse_book = function(input = '.', output = NULL, envir = parent.frame()) {
  # when input is c(dir, file1, file2, ...), we find book files under dir, but
  # only preview file1, file2, ...
  if (dir.exists(input[1])) {
    preview = input[-1]; input = input[1]
  } else preview = NULL
  # automatically search for book files if input is dir
  if (auto <- length(input) == 1 && dir.exists(input)) input = find_input(input)
  if (length(input) == 0) stop('No input was provided or found.')
  # detect output format and config
  text = read_utf8(input[1]); part = yaml_body(text); yaml = part$yaml
  format = detect_format(output, yaml)
  output = auto_output(input[1], output, format)
  cfg = merge_list(list(
    new_session = FALSE, subdir = FALSE,
    chapter_begin = '', chapter_end = "Source: `$input$`"
  ), yaml[['litedown']])
  # if subdir = TRUE but input doesn't include files from subdir, redo the search
  if (auto && isTRUE(cfg$subdir) && !any(grepl('/', input))) {
    input = find_input(dirname(input[1]), TRUE)
  }

  res = lapply(preview %|% input, function(x) {
    out = if (grepl('[.]md$', x)) read_utf8(x) else {
      fmt = paste0('markdown:', format)  # generate intermediate markdown output
      if (cfg$new_session) {
        xfun::Rscript_call(litedown::fuse, list(x, fmt))
      } else {
        fuse(x, fmt, NULL, envir)
      }
    }
    # remove YAML in the preview mode since we only need the body
    if (length(preview)) out = xfun::yaml_body(split_lines(out))$body

    if (format != 'html') return(out)
    # add input filenames to the end for HTML output and wrap each file in a div
    info = function(cls) c(
      sprintf('::: {.chapter-%s .side .side-right}', cls),
      sub_vars(cfg[[sprintf('chapter_%s', cls)]], list(input = I(x))), ':::'
    )
    # for the first input, the fenced Divs should be inserted after YAML
    h = if (length(preview) == 0 && 'yaml' %in% names(part) && x == input[1]) {
      out = split_lines(out)
      if (length(i <- xfun:::locate_yaml(out)) >= 2) {
        i = seq_len(i[2]); h = out[i]; out = out[-i]
        h
      }
    }
    c(
      h, sprintf('::: {.chapter .body data-source="%s"}', x),
      info('begin'), '', out, '', info('end'), ':::'
    )
  })
  tweak_options(format, yaml, list(
    body_class = '', css = c("@default", "@article"),
    js = c("@sidenotes", "@appendix")
  ), toc = length(preview) == 0)
  fuse_output(input[1], output, unlist(res))
}

# find input files under a directory
find_input = function(d, deep = grepl('/$', d)) {
  x = list.files(d, '[.][Rq]?md$', full.names = TRUE, recursive = deep)
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
  i = sans_ext(basename(x)) == 'index'
  index = x[i][which.min(nchar(x[i]))]  # take the shortest path
  c(index, setdiff(x, index))
}

# temporarily set global metadata and options (inheriting from index.Rmd)
tweak_options = function(format, yaml, meta = NULL, toc = TRUE, options = NULL) {
  nms = paste0('litedown.', format, c('.meta', '.options'))
  defaults = list(
    merge_list(
      .Options[[nms[1]]], meta, yaml_field(yaml, format, 'meta')
    ),
    merge_list(
      .Options[[nms[2]]], options,
      list(toc = toc, number_sections = TRUE, embed_resources = FALSE)
    )
  )
  names(defaults) = nms
  opts = options(defaults)
  xfun::exit_call(function() options(opts))
}

# TODO: fuse() files individually into .html; use meta variables header-includes
# / include-before / include-after to customize <head>, nav bar, footer; it may
# be tricky to resolve relative paths (e.g., for nav links)
fuse_site = function() {
}
