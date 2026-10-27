# Bindings to the vendored cmark-gfm C library. These mirror the API of the
# 'commonmark' package (from which the C code and R wrappers are derived) so
# that litedown can call them internally without depending on 'commonmark'.

#' @useDynLib litedown R_list_extensions R_render_markdown R_parse_markdown R_code_tokens R_prose_lines R_convert_attrs
NULL

# Extensions that used to be unconditional R post-processing and must now always
# be enabled at the C level, regardless of the caller's `extensions` argument, so
# that direct markdown_*() callers (not just mark()) get them. Keyed by output
# format. These are hidden from users (excluded in markdown_options()):
#   rawblock     raw content blocks ```{=html} / ```{=latex} / ```{=tex}
#   inlineattrs  {#id .class key=val} on links and images
#   attributes   {.class #id key=val} on fenced code blocks (html only)
# Extensions that are enabled conditionally (math via latex_math, latexfootnotes
# via footnotes) are NOT here; mark() adds those based on the relevant options.
always_on = function(format = 'html') {
  c('rawblock', 'inlineattrs', if (format == 'html') 'attributes')
}

# call the C renderer; `type` is the cmark output format id, and the markdown_*()
# wrappers below only differ in the arguments they expose (the rest are defaults)
render_markdown = function(
  text, type, extensions, always = NULL, hardbreaks = FALSE, smart = FALSE,
  normalize = FALSE, sourcepos = FALSE, footnotes = FALSE, width = 0
) {
  text = enc2utf8(paste(text, collapse = '\n'))
  extensions = union(get_extensions(extensions), always)
  .Call(
    R_render_markdown, text, type, sourcepos, hardbreaks, smart, normalize,
    footnotes, as.integer(width), extensions
  )
}

markdown_html = function(
  text, hardbreaks = FALSE, smart = FALSE, normalize = FALSE, sourcepos = FALSE,
  footnotes = FALSE, extensions = FALSE
) {
  render_markdown(
    text, 1L, extensions, always_on('html'), hardbreaks, smart, normalize,
    sourcepos, footnotes
  )
}

markdown_xml = function(
  text, hardbreaks = FALSE, smart = FALSE, normalize = FALSE, sourcepos = FALSE,
  footnotes = FALSE, extensions = FALSE
) {
  render_markdown(
    text, 2L, extensions, NULL, hardbreaks, smart, normalize, sourcepos, footnotes
  )
}

# man, commonmark, text, and latex take `width` instead of `sourcepos`
markdown_width = function(type, always = NULL) {
  function(
    text, hardbreaks = FALSE, smart = FALSE, normalize = FALSE, footnotes = FALSE,
    width = 0, extensions = FALSE
  ) {
    render_markdown(
      text, type, extensions, always, hardbreaks, smart, normalize,
      footnotes = footnotes, width = width
    )
  }
}
markdown_man = markdown_width(3L)
markdown_commonmark = markdown_width(4L)
markdown_text = markdown_width(5L)
markdown_latex = markdown_width(6L, always_on('latex'))

# Parse markdown and return the cmark AST as a nested list. Each node is a list
# with fields: type, sourcepos (start_line, start_column, end_line, end_column),
# literal, info, level, url, title, and children.
markdown_ast = function(
  text, hardbreaks = FALSE, smart = FALSE, normalize = FALSE, footnotes = FALSE,
  extensions = FALSE
) {
  text = enc2utf8(paste(text, collapse = '\n'))
  extensions = get_extensions(extensions)
  .Call(R_parse_markdown, text, hardbreaks, smart, normalize, footnotes, 0L, extensions)
}

# collect code blocks and inline code from the parse tree as a data frame with
# columns type, start_line, start_col, end_line, end_col, info, literal
markdown_code_tokens = function(
  text, hardbreaks = FALSE, smart = FALSE, normalize = FALSE, footnotes = FALSE,
  extensions = FALSE
) {
  text = enc2utf8(paste(text, collapse = '\n'))
  extensions = get_extensions(extensions)
  .Call(R_code_tokens, text, hardbreaks, smart, normalize, footnotes, extensions)
}

list_extensions = function() .Call(R_list_extensions)

# Indices of the "prose" elements of a character vector (one element per line):
# those not inside a code block. Backed by a single cmark parse in C (exact
# block structure, including indented code and unbalanced fences), replacing the
# fragile/slow regex in xfun::prose_index().
prose_index = function(text) {
  if (length(text) == 0) return(integer())
  .Call(R_prose_lines, enc2utf8(as.character(text)))
}

get_extensions = function(x) {
  if (identical(x, FALSE)) return(NULL)
  exts = list_extensions()
  if (isTRUE(x)) return(exts)
  found = match(x, exts)
  if (any(unfound <- is.na(found))) stop(
    'Invalid commonmark extensions: ', paste(x[unfound], collapse = ', '), call. = FALSE
  )
  x
}
