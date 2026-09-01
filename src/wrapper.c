/** cmark binding for R by Jeroen Ooms (2021)
 *  Adapted from CLI in main.c
 *  NOTE: man(3) states that cmark assumes UTF-8 for both input and output
 */

#include <Rinternals.h>
#include <stdlib.h>

/* Github extensions */
#include "parser.h"

typedef enum {
  FORMAT_NONE,
  FORMAT_HTML,
  FORMAT_XML,
  FORMAT_MAN,
  FORMAT_COMMONMARK,
  FORMAT_PLAINTEXT,
  FORMAT_LATEX
} writer_format;

static char* print_document(cmark_node *document, cmark_parser *parser, writer_format writer, int options, int width){
  switch (writer) {
  case FORMAT_HTML:
    return cmark_render_html(document, options, parser->syntax_extensions);
  case FORMAT_XML:
    return cmark_render_xml(document, options);
  case FORMAT_MAN:
    return cmark_render_man(document, options, width);
  case FORMAT_COMMONMARK:
    return cmark_render_commonmark(document, options, width);
  case FORMAT_LATEX:
    return cmark_render_latex(document, options, width);
  case FORMAT_PLAINTEXT:
    return cmark_render_plaintext(document, options, width);
  default:
    Rf_error("Unknown output format %d", writer);
  }
}

SEXP R_render_markdown(SEXP text, SEXP format, SEXP sourcepos, SEXP hardbreaks, SEXP smart,
                       SEXP normalize, SEXP footnotes, SEXP width, SEXP extensions) {

  /* input validation */
  if(!Rf_isString(text))
    Rf_error("Argument 'text' must be string.");
  if(!Rf_isInteger(format))
    Rf_error("Argument 'format' must be integer.");
  if(!Rf_isLogical(sourcepos))
    Rf_error("Argument 'sourcepos' must be logical.");
  if(!Rf_isLogical(hardbreaks))
    Rf_error("Argument 'hardbreaks' must be logical.");
  if(!Rf_isLogical(smart))
    Rf_error("Argument 'smart' must be logical.");
  if(!Rf_isLogical(normalize))
    Rf_error("Argument 'normalize' must be logical.");
  if(!Rf_isLogical(footnotes))
    Rf_error("Argument 'footnotes' must be logical.");
  if(!Rf_isInteger(width))
    Rf_error("Argument 'width' must be integer.");

  /* combine options */
  int options = CMARK_OPT_DEFAULT;
  options += CMARK_OPT_STRIKETHROUGH_DOUBLE_TILDE;
  options += Rf_asLogical(sourcepos) * CMARK_OPT_SOURCEPOS;
  options += Rf_asLogical(hardbreaks) * CMARK_OPT_HARDBREAKS;
  options += Rf_asLogical(smart) * CMARK_OPT_SMART;
  options += Rf_asLogical(normalize) * CMARK_OPT_NORMALIZE;
  options += Rf_asLogical(footnotes) * CMARK_OPT_FOOTNOTES;

  /* Prevent filtering embedded resources: https://github.com/github/cmark-gfm#security */
  options += CMARK_OPT_UNSAFE;

  /* parse input */
  SEXP input = STRING_ELT(text, 0);
  cmark_parser *parser = cmark_parser_new(options);
  for(int i = 0; i < Rf_length(extensions); i++){
    const char *extname = CHAR(STRING_ELT(extensions, i));
    cmark_syntax_extension *syntax_extension = cmark_find_syntax_extension(extname);
    if(!syntax_extension)
      Rf_error("Failed to load extension '%s'", extname);
    cmark_parser_attach_syntax_extension(parser, syntax_extension);
  }
  cmark_parser_feed(parser, CHAR(input), LENGTH(input));
  cmark_node *doc = cmark_parser_finish(parser);

  /* render output format */
  char *output = print_document(doc, parser, Rf_asInteger(format), options, Rf_asInteger(width));
  cmark_parser_free(parser);
  cmark_node_free(doc);

  /* cmark always returns UTF8 output */
  SEXP res = PROTECT(Rf_allocVector(STRSXP, 1));
  SET_STRING_ELT(res, 0, Rf_mkCharCE(output, CE_UTF8));
  UNPROTECT(1);
  free(output);
  return res;
}
