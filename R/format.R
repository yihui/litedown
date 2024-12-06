is_rmd_preview = function() Sys.getenv('RMARKDOWN_PREVIEW_DIR') != ''

output_format = function(to, options, meta, ...) {
  if (is_rmd_preview()) stop(
    "It appears that you clicked the 'Knit' button in RStudio to render the document. ",
    "You are recommended to use litedown::roam() to preview or render documents instead. ",
    "Alternatively, you can add a top-level field 'knit: litedown:::knit' to the YAML metadata, ",
    "so the document can be rendered by litedown::fuse() instead of rmarkdown::render().",
    call. = FALSE
  )
  msg = 'Please render the document via litedown::fuse() instead of rmarkdown::render().'
  if ('pkgdown' %in% loadedNamespaces()) {
    warning(
      msg, '\n\nIf you intend to build a package website, you can also use litedown:',
      ' https://yihui.org/litedown/#sec:pkg-site', call. = FALSE
    )
    ns = asNamespace('rmarkdown')
    ag = merge_list(list(to = to), list(...))
    ag = ag[intersect(names(formals(ns$pandoc_options)), names(ag))]
    opts = do.call(ns$pandoc_options, ag)
    return(ns$output_format(NULL, opts))
  }
  stop(msg, call. = FALSE)
}

#' Output formats in YAML metadata
#'
#' These functions exist only for historical reasons, and should never be called
#' directly. They can be used to configure output formats in YAML, but you are
#' recommended to use the file format names instead of these function names.
#'
#' To configure output formats in the YAML metadata of the Markdown document,
#' simply use the output format names such as `html` or `latex` in the `output`
#' field in YAML, e.g.,
#'
#' ```yaml
#' ---
#' output:
#'   html:
#'     options:
#'       toc: true
#'     keep_md: true
#'   latex:
#'     latex_engine: pdflatex
#' ---
#' ```
#'
#' You can also use `litedown::html_format` instead of `html` (or
#' `litedown::latex_format` instead of `latex`) if you like.
#' @param meta,options Arguments to be passed to [mark()].
#' @param template A template file path.
#' @param keep_md,keep_tex Whether to keep the intermediate \file{.md} and
#'   \file{.tex} files generated from \file{.Rmd}.
#' @param latex_engine The LaTeX engine to compile \file{.tex} to \file{.pdf}.
#' @param citation_package The LaTeX package for processing citations. Possible
#'   values are `none`, `natbib`, and `biblatex`.
#' @note If you want to use the `Knit` button in RStudio, you must add a
#'   top-level field `knit: litedown:::knit` to the YAML metadata. See
#'   \url{https://yihui.org/litedown/#sec:knit-button} for more information.
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
  output_format()
}

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
  res = xfun::yaml_body(text, use_yaml = FALSE)
  if (length(out <- res$yaml[['output']]) == 0) return(res)
  if (is.character(out)) out = set_names(vector('list', length(out)), out)
  if (!is.list(out)) stop('The output field in YAML must be either list or character')
  fmt = c(html_document = 'html', html_vignette = 'html', pdf_document = 'latex')
  for (i in intersect(names(fmt), names(out))) {
    out[[i]] = if (is.list(out[[i]])) do.call(map_args, out[[i]]) else list()
    names(out)[names(out) == i] = fmt[i]
  }
  # normalize format names `litedown::*_format` to `*`
  names(out) = gsub('^litedown::+([^_]+)_.*', '\\1', names(out))
  res$yaml$output = out
  res
}

# get metadata from a certain field under an output format
yaml_field = function(yaml, format, name = 'meta') {
  if (is.list(out <- yaml[['output']]) && is.list(out <- out[[format]])) {
    if (length(name) == 1) out[[name]] else out[name]
  }
}

# get output format from YAML's `output` field
yaml_format = function(yaml) {
  if (is.list(out <- yaml[['output']])) out = names(out)
  c(out, 'html')[1]
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
  markdown = '.md', text = '.txt', latex = '.tex'
)
