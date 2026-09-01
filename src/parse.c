/* Parse markdown to an R representation of the cmark AST.
 *
 * Unlike R_render_markdown() (which renders to a string), R_parse_markdown()
 * returns the parse tree so that R code can walk nodes directly instead of
 * parsing rendered XML/HTML with regular expressions.
 */

#include <Rinternals.h>
#include <stdlib.h>
#include <string.h>
#include "parser.h"

/* Build a nested R list for a node and all of its descendants. Each node is a
 * named list with fields:
 *   type       : node type string (e.g. "document", "code_block", "code")
 *   sourcepos  : integer(4) c(start_line, start_column, end_line, end_column)
 *   literal    : the literal content (code, text, html), or NA if none
 *   info       : fenced code block info string, or NA
 *   level      : heading level (integer), or NA
 *   url        : link/image destination, or NA
 *   title      : link/image title, or NA
 *   children   : list of child nodes (possibly empty)
 */
static SEXP build_node(cmark_node *node) {
  const int n_fields = 8;
  SEXP out = PROTECT(Rf_allocVector(VECSXP, n_fields));
  SEXP nms = PROTECT(Rf_allocVector(STRSXP, n_fields));
  SET_STRING_ELT(nms, 0, Rf_mkChar("type"));
  SET_STRING_ELT(nms, 1, Rf_mkChar("sourcepos"));
  SET_STRING_ELT(nms, 2, Rf_mkChar("literal"));
  SET_STRING_ELT(nms, 3, Rf_mkChar("info"));
  SET_STRING_ELT(nms, 4, Rf_mkChar("level"));
  SET_STRING_ELT(nms, 5, Rf_mkChar("url"));
  SET_STRING_ELT(nms, 6, Rf_mkChar("title"));
  SET_STRING_ELT(nms, 7, Rf_mkChar("children"));
  Rf_setAttrib(out, R_NamesSymbol, nms);
  UNPROTECT(1);  /* nms is now protected via out */

  /* type */
  SET_VECTOR_ELT(out, 0, Rf_mkString(cmark_node_get_type_string(node)));

  /* sourcepos */
  SEXP pos = PROTECT(Rf_allocVector(INTSXP, 4));
  INTEGER(pos)[0] = cmark_node_get_start_line(node);
  INTEGER(pos)[1] = cmark_node_get_start_column(node);
  INTEGER(pos)[2] = cmark_node_get_end_line(node);
  INTEGER(pos)[3] = cmark_node_get_end_column(node);
  SET_VECTOR_ELT(out, 1, pos);
  UNPROTECT(1);

  /* literal */
  const char *literal = cmark_node_get_literal(node);
  SET_VECTOR_ELT(out, 2, literal ?
    Rf_ScalarString(Rf_mkCharCE(literal, CE_UTF8)) : Rf_ScalarString(NA_STRING));

  /* fenced code info string (only meaningful for code blocks) */
  const char *info = NULL;
  cmark_node_type type = cmark_node_get_type(node);
  if (type == CMARK_NODE_CODE_BLOCK)
    info = cmark_node_get_fence_info(node);
  SET_VECTOR_ELT(out, 3, (info && info[0]) ?
    Rf_ScalarString(Rf_mkCharCE(info, CE_UTF8)) : Rf_ScalarString(NA_STRING));

  /* heading level */
  if (type == CMARK_NODE_HEADING)
    SET_VECTOR_ELT(out, 4, Rf_ScalarInteger(cmark_node_get_heading_level(node)));
  else
    SET_VECTOR_ELT(out, 4, Rf_ScalarInteger(NA_INTEGER));

  /* url / title (links and images) */
  const char *url = cmark_node_get_url(node);
  SET_VECTOR_ELT(out, 5, (url && url[0]) ?
    Rf_ScalarString(Rf_mkCharCE(url, CE_UTF8)) : Rf_ScalarString(NA_STRING));
  const char *title = cmark_node_get_title(node);
  SET_VECTOR_ELT(out, 6, (title && title[0]) ?
    Rf_ScalarString(Rf_mkCharCE(title, CE_UTF8)) : Rf_ScalarString(NA_STRING));

  /* children: count first, then fill */
  int n = 0;
  for (cmark_node *c = cmark_node_first_child(node); c; c = cmark_node_next(c))
    n++;
  SEXP kids = PROTECT(Rf_allocVector(VECSXP, n));
  int i = 0;
  for (cmark_node *c = cmark_node_first_child(node); c; c = cmark_node_next(c)) {
    SEXP child = build_node(c);
    SET_VECTOR_ELT(kids, i++, child);
    UNPROTECT(1);  /* child is now protected via kids */
  }
  SET_VECTOR_ELT(out, 7, kids);
  UNPROTECT(1);  /* kids */

  /* returns with 'out' still protected; the caller is responsible for it */
  return out;
}

SEXP R_parse_markdown(SEXP text, SEXP hardbreaks, SEXP smart, SEXP normalize,
                      SEXP footnotes, SEXP width, SEXP extensions) {
  if (!Rf_isString(text))
    Rf_error("Argument 'text' must be string.");
  if (!Rf_isLogical(hardbreaks))
    Rf_error("Argument 'hardbreaks' must be logical.");
  if (!Rf_isLogical(smart))
    Rf_error("Argument 'smart' must be logical.");
  if (!Rf_isLogical(normalize))
    Rf_error("Argument 'normalize' must be logical.");
  if (!Rf_isLogical(footnotes))
    Rf_error("Argument 'footnotes' must be logical.");
  (void) width;

  int options = CMARK_OPT_DEFAULT;
  options += CMARK_OPT_SOURCEPOS;
  options += CMARK_OPT_STRIKETHROUGH_DOUBLE_TILDE;
  options += Rf_asLogical(hardbreaks) * CMARK_OPT_HARDBREAKS;
  options += Rf_asLogical(smart) * CMARK_OPT_SMART;
  options += Rf_asLogical(normalize) * CMARK_OPT_NORMALIZE;
  options += Rf_asLogical(footnotes) * CMARK_OPT_FOOTNOTES;
  options += CMARK_OPT_UNSAFE;

  SEXP input = STRING_ELT(text, 0);
  cmark_parser *parser = cmark_parser_new(options);
  for (int i = 0; i < Rf_length(extensions); i++) {
    const char *extname = CHAR(STRING_ELT(extensions, i));
    cmark_syntax_extension *ext = cmark_find_syntax_extension(extname);
    if (!ext) {
      cmark_parser_free(parser);
      Rf_error("Failed to load extension '%s'", extname);
    }
    cmark_parser_attach_syntax_extension(parser, ext);
  }
  cmark_parser_feed(parser, CHAR(input), LENGTH(input));
  cmark_node *doc = cmark_parser_finish(parser);

  /* build_node() returns an already-protected value */
  SEXP res = build_node(doc);

  cmark_parser_free(parser);
  cmark_node_free(doc);
  UNPROTECT(1);
  return res;
}
