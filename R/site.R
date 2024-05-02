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
#'     chapter_end: "This chapter was generated from `$input$`."
#' ---
#' ```
#'
#' The option `new_session` specifies whether to render each input file in the
#' current R session or a separate new R session; `chapter_end` specifies
#' additional text to be appended to the content of each file, which accepts
#' some variables (e.g., `$input$` is the current input file path).
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
  if (length(input) == 1 && dir.exists(input)) input = find_input(input)
  if (length(input) == 0) stop('No input was provided or found.')
  input = reorder_input(input)
  input = sub('^[.]/+', '', input)
  # detect output format and config
  text = read_utf8(input[1]); part = yaml_body(text); yaml = part$yaml
  format = detect_format(output, yaml)
  output = auto_output(input[1], output, format)
  cfg = yaml[['litedown']]
  new_session = cfg[['new_session']] %||% FALSE
  chapter_end = cfg[['chapter_end']] %||% "Source: `$input$`"
  res = lapply(input, function(x) {
    out = if (grepl('[.]md$', x)) read_utf8(x) else {
      fmt = paste0('markdown:', format)  # generate intermediate markdown output
      if (new_session) {
        xfun::Rscript_call(litedown::fuse, list(x, fmt))
      } else {
        fuse(x, fmt, NULL, envir)
      }
    }
    if (format != 'html') return(out)
    # add input filenames to the end for HTML output and wrap each file in a div
    c(
      '::: {.chapter .body}', out, '', '::: chapter-end',
      sub_vars(chapter_end, list(input = I(x))), ':::', ':::'
    )
  })
  res = split_lines(unlist(res))
  # for the first input, the container starting fence should be moved after YAML
  if ('yaml' %in% names(part) && length(i <- xfun:::locate_yaml(res[-1])) >= 2)
    res = append(res[-1], res[1], i[2] + 1)
  tweak_options(format, yaml, list(
    body_class = '', css = c("@default", "@article"),
    js = c("@sidenotes", "@appendix")
  ))
  fuse_output(input[1], output, res)
}

# find input files under a directory
find_input = function(d) {
  x = list.files(d, '[.][Rq]?md$', full.names = TRUE, recursive = grepl('/$', d))
  # exclude .* and _* files
  x = x[!grepl('^[_.]', basename(x))]
  # exclude readme
  x = x[tolower(basename(sans_ext(x))) != 'readme']
  # for .md files, don't include them if they have .Rmd/.qmd files
  b = sans_ext(x); i = file_ext(x) == 'md'
  x = x[!i | !(b %in% sub('[.][Rq]md$', '', x))]
  x
}

# move index.[Rq]md to the first
reorder_input = function(x) {
  i = sans_ext(basename(x)) == 'index'
  index = x[i][which.min(nchar(x[i]))]  # take the shortest path
  c(index, setdiff(x, index))
}

# temporarily set global metadata and options (inheriting from index.Rmd)
tweak_options = function(format, yaml, meta = NULL, options = NULL) {
  nms = paste0('litedown.', format, c('.meta', '.options'))
  defaults = list(
    merge_list(
      meta, yaml_field(yaml, format, 'meta'), .Options[[nms[1]]]
    ),
    merge_list(
      options,
      list(toc = TRUE, number_sections = TRUE, embed_resources = FALSE),
      .Options[[nms[2]]]
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