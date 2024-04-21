output_format = function(to, options, meta, template, keep_md, ...) {
  opts = rmarkdown::pandoc_options(to = to, ...)
  opts$convert_fun = function(input, output, ...) {
    mark(input, output, options, meta, text = NULL)
  }
  rmarkdown::output_format(
    NULL, opts, keep_md = keep_md,
    pre_processor = function(meta, input, runtime, knit_meta, ...) {
      # knitr::knit_meta() has been emptied at this stage and only available in
      # the `knit_meta` argument; make a copy in .env so that it can be accessed
      # in add_html_deps() later
      .env$knit_meta = knit_meta; NULL
    },
    on_exit = function() .env$knit_meta = NULL,
    clean_supporting = 'local' %in% normalize_options(options)[['embed_resources']]
  )
}

#' Output formats in YAML metadata
#'
#' The primary output formats of \pkg{litedown} are HTML and LaTeX. These output
#' formats can be configured in the `output` field of the YAML metadata of the
#' Markdown document.
#'
#' The \pkg{rmarkdown} package converts Markdown using Pandoc by default, and it
#' also accept custom converting tools. The output formats here provide the
#' custom converting function [mark()] to \pkg{rmarkdown}, so that users can
#' take advantage of [rmarkdown::render()] and the Knit button in RStudio. It is
#' absolutely not necessary to rely on \pkg{rmarkdown}. The only point is
#' convenience. If you do not use `rmarkdown::render()` or the Knit button, you
#' can definitely just call `litedown::mark()` directly.
#' @param meta,options Arguments to be passed to [mark()].
#' @param template A template file path.
#' @param keep_md,keep_tex Whether to keep the intermediate \file{.md} and
#'   \file{.tex} files generated from \file{.Rmd}.
#' @param latex_engine The LaTeX engine to compile \file{.tex} to \file{.pdf}.
#' @export
html_format = function(options = NULL, meta = NULL, template = NULL, keep_md = FALSE) {
  output_format('html', options, meta, template, keep_md)
}

#' @rdname html_format
#' @export
latex_format = function(
  options = NULL, meta = NULL, template = NULL, keep_md = FALSE,
  keep_tex = FALSE, latex_engine = 'xelatex'
) {
  output_format(
    'latex', options, meta, template, keep_md,
    keep_tex = keep_tex, latex_engine = latex_engine
  )
}

# compatibility layers to rmarkdown::[html|pdf]_document
html_document = function(...) do.call(html_format, map_args(...))
html_vignette = function(...) html_document(...)
pdf_document = function(...) do.call(latex_format, map_args(...))

# map rmarkdown arguments to markdown
map_args = function(
  toc = FALSE, toc_depth = 3, number_sections = FALSE, anchor_sections = FALSE,
  code_folding = 'none', self_contained = TRUE, math_method = 'default',
  css = NULL, includes = NULL, ...
) {
  opts = list(
    toc = toc, number_sections = number_sections, embed_resources = self_contained
  )
  meta = list(css = c('default', css))
  if (toc) opts$toc = list(depth = toc_depth)
  if (identical(
    if (is.list(math_method)) math_method$engine else math_method, 'mathjax'
  )) opts$js_math = 'mathjax'
  if (!isFALSE(anchor_sections)) {
    meta$js = c(meta$js, '@heading-anchor')
    meta$css = c(meta$css, '@heading-anchor')
  }
  # 'hide' is not supported here; if it is desired, use <script data-open=false>
  if (code_folding != 'none') meta$js = c(
    meta$js, '@fold-details'
  )
  if (is.list(includes)) meta[
    c('header_includes', 'include_before', 'include_after')
  ] = includes[c('in_header', 'before_body', 'after_body')]
  list(meta = meta, options = opts, ...)
}

# get metadata from a certain field under an output format
yaml_field = function(yaml, format, name = 'meta') {
  if (!is.list(out <- yaml[['output']])) return()
  if (format == 'latex') format = '(latex|pdf)'
  # try any (html|latex)_* output format
  i = grep(sprintf('^litedown:::?%s_', format), names(out), value = TRUE)[1]
  if (!is.list(out <- out[[i]])) return()
  # compatibility with rmarkdown::(html|latex|pdf)_document
  if (!grepl('_format$', i)) out = do.call(map_args, out)
  out[[name]]
}

# get output format from YAML's `output` field
yaml_format = function(yaml) {
  if (is.list(out <- yaml[['output']])) out = names(out)
  out = xfun::grep_sub('^litedown:::?([^_]+)_.*', '\\1', out)
  if (length(out) < 1) 'html' else out[1]
}

# determine output format based on output file name and input's YAML
detect_format = function(output, yaml) {
  res = if (is.character(output)) {
    if (output %in% names(md_formats)) output else {
      ext = xfun::file_ext(output)
      if (ext == 'pdf') 'latex' else names(which(md_formats == paste0('.', ext)))
    }
  }
  if (length(res) == 1) res else yaml_format(yaml)
}

md_formats = c(
  html = '.html', xml = '.xml', man = '.man', commonmark = '.markdown',
  text = '.txt', latex = '.tex'
)
