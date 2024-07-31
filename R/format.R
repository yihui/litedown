is_rmd_preview = function() Sys.getenv('RMARKDOWN_PREVIEW_DIR') != ''

output_format = function(to, options, meta, template, keep_md, ...) {
  if (is_rmd_preview()) xfun::do_once(message(
    "It appears that you clicked the 'Knit' button in RStudio to render the document, ",
    "but perhaps should add a top-level field 'knit: litedown:::knit' to the YAML metadata, ",
    "so the document can be rendered by litedown::fuse() instead of rmarkdown::render().\n"
  ), 'litedown.rmarkdown.reminder')
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
#' The output format functions have two purposes. The main purpose is to make it
#' possible (and easier) to configure the output formats using YAML metadata
#' inside a document, e.g.,
#'
#' ```yaml
#' ---
#' output:
#'   litedown::html_format:
#'     options:
#'       toc: true
#'     keep_md: true
#'   litedown::latex_format:
#'     latex_engine: pdflatex
#' ---
#' ```
#'
#' The secondary purpose is for \pkg{rmarkdown} users to render R Markdown via
#' [knitr::knit()] and [mark()] (instead of Pandoc), and also use the `Knit`
#' button in RStudio. Although you can render R Markdown to Markdown via either
#' `knitr::knit()` or [fuse()], please note that the two ways are not 100%
#' compatible with each other. If you choose to use \pkg{litedown}, we recommend
#' that you use `fuse()` instead. If you want `fuse()` to work with the `Knit`
#' button in RStudio, you have to add a special field to YAML:
#'
#' ```yaml
#' ---
#' knit: litedown:::knit
#' ---
#' ```
#'
#' Without this field, RStudio will use \pkg{knitr} to render R Markdown.
#' @param meta,options Arguments to be passed to [mark()].
#' @param template A template file path.
#' @param keep_md,keep_tex Whether to keep the intermediate \file{.md} and
#'   \file{.tex} files generated from \file{.Rmd}.
#' @param latex_engine The LaTeX engine to compile \file{.tex} to \file{.pdf}.
#' @param citation_package The LaTeX package for processing citations. Possible
#'   values are `none`, `natbib`, and `biblatex`.
#' @return An R Markdown output format.
#' @export
html_format = function(options = NULL, meta = NULL, template = NULL, keep_md = FALSE) {
  output_format('html', options, meta, template, keep_md)
}

#' @rdname html_format
#' @export
latex_format = function(
  options = NULL, meta = NULL, template = NULL, keep_md = FALSE,
  keep_tex = FALSE, latex_engine = 'xelatex', citation_package = 'natbib'
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

# split YAML and body from text input, and normalize rmarkdown output formats in
# YAML to litedown's formats
yaml_body = function(text) {
  res = xfun::yaml_body(text)
  if (length(out <- res$yaml[['output']]) == 0 || !is.list(out)) return(res)
  fmt = c(
    html_document = 'litedown::html_format',
    html_vignette = 'litedown::html_format',
    pdf_document = 'litedown::latex_format'
  )
  for (i in intersect(names(fmt), names(out))) {
    out[[i]] = if (is.list(out[[i]])) do.call(map_args, out[[i]]) else list()
    names(out)[names(out) == i] = fmt[i]
  }
  res$yaml$output = out
  res
}

# get metadata from a certain field under an output format
yaml_field = function(yaml, format, name = 'meta') {
  i = sprintf('litedown::%s_format', format)
  if (is.list(out <- yaml[['output']]) && is.list(out <- out[[i]])) {
    if (length(name) == 1) out[[name]] else out[name]
  }
}

# get output format from YAML's `output` field
yaml_format = function(yaml) {
  if (is.list(out <- yaml[['output']])) out = names(out)
  out = grep_sub('^litedown::([^_]+)_.*', '\\1', out)
  if (length(out) < 1) 'html' else out[1]
}

# determine output format based on output file name and input's YAML
detect_format = function(output, yaml) {
  res = if (is.character(output)) {
    # check if output is a known format, e.g., output = 'html'
    if (output %in% names(md_formats)) output else {
      # check file extension, e.g., output = '.pdf'
      ext = file_ext(output)
      if (ext == 'pdf') 'latex' else {
        names(which(md_formats == paste0('.', ext))) %|%
          # output = 'markdown:format', e.g., markdown:latex means the final
          # format is latex but the intermediate output should be markdown
          if (startsWith(output, 'markdown:')) intersect(
            sub('^markdown:', '', output), names(md_formats)
          )
      }
    }
  }
  # if unable to detect format from `output`, try YAML
  if (length(res) == 1) res else yaml_format(yaml)
}

md_formats = c(
  html = '.html', xml = '.xml', man = '.man', commonmark = '.markdown',
  text = '.txt', latex = '.tex'
)
