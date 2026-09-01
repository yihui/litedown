# Bindings to the vendored cmark-gfm C library. These mirror the API of the
# 'commonmark' package (from which the C code and R wrappers are derived) so
# that litedown can call them internally without depending on 'commonmark'.

#' @useDynLib litedown R_list_extensions R_render_markdown R_parse_markdown R_code_tokens
NULL

markdown_html = function(
  text, hardbreaks = FALSE, smart = FALSE, normalize = FALSE, sourcepos = FALSE,
  footnotes = FALSE, extensions = FALSE
) {
  text = enc2utf8(paste(text, collapse = '\n'))
  extensions = get_extensions(extensions)
  .Call(R_render_markdown, text, 1L, sourcepos, hardbreaks, smart, normalize, footnotes, 0L, extensions)
}

markdown_xml = function(
  text, hardbreaks = FALSE, smart = FALSE, normalize = FALSE, sourcepos = FALSE,
  footnotes = FALSE, extensions = FALSE
) {
  text = enc2utf8(paste(text, collapse = '\n'))
  extensions = get_extensions(extensions)
  .Call(R_render_markdown, text, 2L, sourcepos, hardbreaks, smart, normalize, footnotes, 0L, extensions)
}

markdown_man = function(
  text, hardbreaks = FALSE, smart = FALSE, normalize = FALSE, footnotes = FALSE,
  width = 0, extensions = FALSE
) {
  text = enc2utf8(paste(text, collapse = '\n'))
  extensions = get_extensions(extensions)
  .Call(R_render_markdown, text, 3L, FALSE, hardbreaks, smart, normalize, footnotes, as.integer(width), extensions)
}

markdown_commonmark = function(
  text, hardbreaks = FALSE, smart = FALSE, normalize = FALSE, footnotes = FALSE,
  width = 0, extensions = FALSE
) {
  text = enc2utf8(paste(text, collapse = '\n'))
  extensions = get_extensions(extensions)
  .Call(R_render_markdown, text, 4L, FALSE, hardbreaks, smart, normalize, footnotes, as.integer(width), extensions)
}

markdown_text = function(
  text, hardbreaks = FALSE, smart = FALSE, normalize = FALSE, footnotes = FALSE,
  width = 0, extensions = FALSE
) {
  text = enc2utf8(paste(text, collapse = '\n'))
  extensions = get_extensions(extensions)
  .Call(R_render_markdown, text, 5L, FALSE, hardbreaks, smart, normalize, footnotes, as.integer(width), extensions)
}

markdown_latex = function(
  text, hardbreaks = FALSE, smart = FALSE, normalize = FALSE, footnotes = FALSE,
  width = 0, extensions = FALSE
) {
  text = enc2utf8(paste(text, collapse = '\n'))
  extensions = get_extensions(extensions)
  .Call(R_render_markdown, text, 6L, FALSE, hardbreaks, smart, normalize, footnotes, as.integer(width), extensions)
}

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
