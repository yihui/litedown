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
#include "buffer.h"
#include "extensions/litedown-extensions.h"
#include "extensions/attributes.h"

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

/* Parse `text` into a cmark document, attaching the named `extensions`. The
 * caller must free both the returned document (cmark_node_free) and the parser
 * (cmark_parser_free) it stores via `*out_parser`. */
static cmark_node *parse_document(SEXP text, SEXP extensions, int options,
                                  cmark_parser **out_parser) {
  SEXP input = STRING_ELT(text, 0);
  cmark_parser *parser = cmark_parser_new(options);
  for (int i = 0; i < Rf_length(extensions); i++) {
    const char *extname = CHAR(STRING_ELT(extensions, i));
    if (!litedown_attach_extension(parser, extname)) {
      cmark_parser_free(parser);
      Rf_error("Failed to load extension '%s'", extname);
    }
  }
  cmark_parser_feed(parser, CHAR(input), LENGTH(input));
  *out_parser = parser;
  return cmark_parser_finish(parser);
}

static int parse_options(SEXP hardbreaks, SEXP smart, SEXP normalize, SEXP footnotes) {
  if (!Rf_isLogical(hardbreaks) || !Rf_isLogical(smart) ||
      !Rf_isLogical(normalize) || !Rf_isLogical(footnotes))
    Rf_error("Arguments 'hardbreaks', 'smart', 'normalize', 'footnotes' must be logical.");
  int options = CMARK_OPT_DEFAULT;
  options += CMARK_OPT_SOURCEPOS;
  options += Rf_asLogical(hardbreaks) * CMARK_OPT_HARDBREAKS;
  options += Rf_asLogical(smart) * CMARK_OPT_SMART;
  options += Rf_asLogical(normalize) * CMARK_OPT_NORMALIZE;
  options += Rf_asLogical(footnotes) * CMARK_OPT_FOOTNOTES;
  options += CMARK_OPT_UNSAFE;
  return options;
}

SEXP R_parse_markdown(SEXP text, SEXP hardbreaks, SEXP smart, SEXP normalize,
                      SEXP footnotes, SEXP width, SEXP extensions) {
  if (!Rf_isString(text))
    Rf_error("Argument 'text' must be string.");
  (void) width;

  int options = parse_options(hardbreaks, smart, normalize, footnotes);
  cmark_parser *parser;
  cmark_node *doc = parse_document(text, extensions, options, &parser);

  /* build_node() returns an already-protected value */
  SEXP res = build_node(doc);

  cmark_parser_free(parser);
  cmark_node_free(doc);
  UNPROTECT(1);
  return res;
}

/* Collect just the code blocks and inline code from the document into a flat
 * table, which is much cheaper than building the whole AST as a nested R list
 * and walking it in R. Returns a data-frame-ready list of equal-length columns:
 *   type       : "code" (inline) or "code_block" (fenced)
 *   start_line, start_col, end_line, end_col : source position (integers)
 *   info       : fenced code info string (NA for inline code)
 *   literal    : code text
 * crack() uses this to locate code chunks and inline code expressions. */
SEXP R_code_tokens(SEXP text, SEXP hardbreaks, SEXP smart, SEXP normalize,
                   SEXP footnotes, SEXP extensions) {
  if (!Rf_isString(text))
    Rf_error("Argument 'text' must be string.");

  int options = parse_options(hardbreaks, smart, normalize, footnotes);
  cmark_parser *parser;
  cmark_node *doc = parse_document(text, extensions, options, &parser);

  /* first pass: count matching nodes so we can allocate exact-size columns */
  int n = 0;
  cmark_iter *iter = cmark_iter_new(doc);
  cmark_event_type ev;
  while ((ev = cmark_iter_next(iter)) != CMARK_EVENT_DONE) {
    if (ev != CMARK_EVENT_ENTER) continue;
    cmark_node_type t = cmark_node_get_type(cmark_iter_get_node(iter));
    if (t == CMARK_NODE_CODE || t == CMARK_NODE_CODE_BLOCK) n++;
  }
  cmark_iter_free(iter);

  SEXP type    = PROTECT(Rf_allocVector(STRSXP, n));
  SEXP sline   = PROTECT(Rf_allocVector(INTSXP, n));
  SEXP scol    = PROTECT(Rf_allocVector(INTSXP, n));
  SEXP eline   = PROTECT(Rf_allocVector(INTSXP, n));
  SEXP ecol    = PROTECT(Rf_allocVector(INTSXP, n));
  SEXP info    = PROTECT(Rf_allocVector(STRSXP, n));
  SEXP literal = PROTECT(Rf_allocVector(STRSXP, n));

  /* second pass: fill the columns */
  int i = 0;
  iter = cmark_iter_new(doc);
  while ((ev = cmark_iter_next(iter)) != CMARK_EVENT_DONE) {
    if (ev != CMARK_EVENT_ENTER) continue;
    cmark_node *node = cmark_iter_get_node(iter);
    cmark_node_type t = cmark_node_get_type(node);
    if (t != CMARK_NODE_CODE && t != CMARK_NODE_CODE_BLOCK) continue;

    SET_STRING_ELT(type, i, Rf_mkChar(t == CMARK_NODE_CODE ? "code" : "code_block"));
    INTEGER(sline)[i] = cmark_node_get_start_line(node);
    INTEGER(scol)[i]  = cmark_node_get_start_column(node);
    INTEGER(eline)[i] = cmark_node_get_end_line(node);
    INTEGER(ecol)[i]  = cmark_node_get_end_column(node);

    if (t == CMARK_NODE_CODE_BLOCK) {
      const char *fi = cmark_node_get_fence_info(node);
      SET_STRING_ELT(info, i, (fi && fi[0]) ? Rf_mkCharCE(fi, CE_UTF8) : NA_STRING);
    } else {
      SET_STRING_ELT(info, i, NA_STRING);
    }
    const char *lit = cmark_node_get_literal(node);
    SET_STRING_ELT(literal, i, lit ? Rf_mkCharCE(lit, CE_UTF8) : NA_STRING);
    i++;
  }
  cmark_iter_free(iter);
  cmark_parser_free(parser);
  cmark_node_free(doc);

  SEXP out = PROTECT(Rf_allocVector(VECSXP, 7));
  SET_VECTOR_ELT(out, 0, type);
  SET_VECTOR_ELT(out, 1, sline);
  SET_VECTOR_ELT(out, 2, scol);
  SET_VECTOR_ELT(out, 3, eline);
  SET_VECTOR_ELT(out, 4, ecol);
  SET_VECTOR_ELT(out, 5, info);
  SET_VECTOR_ELT(out, 6, literal);
  SEXP nms = PROTECT(Rf_allocVector(STRSXP, 7));
  SET_STRING_ELT(nms, 0, Rf_mkChar("type"));
  SET_STRING_ELT(nms, 1, Rf_mkChar("start_line"));
  SET_STRING_ELT(nms, 2, Rf_mkChar("start_col"));
  SET_STRING_ELT(nms, 3, Rf_mkChar("end_line"));
  SET_STRING_ELT(nms, 4, Rf_mkChar("end_col"));
  SET_STRING_ELT(nms, 5, Rf_mkChar("info"));
  SET_STRING_ELT(nms, 6, Rf_mkChar("literal"));
  Rf_setAttrib(out, R_NamesSymbol, nms);

  UNPROTECT(9);
  return out;
}

/* Return the 1-based indices of the "prose" elements of `text` (a character
 * vector, one element per source line): those not covered by any code block
 * (fenced or indented). This replaces the regex-based xfun::prose_index(): a
 * single cmark parse locates code blocks exactly, including indented code and
 * unbalanced fences (which the regex mis-handles), and is faster.
 *
 * Elements are joined with '\n' exactly as the renderers do before parsing.
 * The join is done here (rather than taking a pre-collapsed string) so the
 * mapping from cmark line numbers back to vector elements stays exact even when
 * an element already contains embedded newlines (e.g. mark() appends "\n" to
 * some lines before calling this): an element that contains c internal newlines
 * spans c+1 lines, and it is prose unless any of those lines is a code line.
 *
 * Only code blocks are excluded (raw HTML blocks such as <pre> or <div> remain
 * prose), matching what the callers need (locating ::: fences, @refs, and
 * smart-typography targets, none of which occur inside code). */
SEXP R_prose_lines(SEXP text) {
  if (!Rf_isString(text))
    Rf_error("Argument 'text' must be string.");

  int n = Rf_length(text);
  if (n == 0)
    return Rf_allocVector(INTSXP, 0);

  /* per-element starting line (1-based) in the joined document, plus the total
   * byte length needed for the join */
  int *start_line = (int *) R_alloc(n, sizeof(int));
  int line = 1;
  size_t total = 0;
  for (int i = 0; i < n; i++) {
    SEXP s = STRING_ELT(text, i);
    const char *d = CHAR(s);
    int len = LENGTH(s), nl = 0;
    start_line[i] = line;
    for (int k = 0; k < len; k++)
      if (d[k] == '\n') nl++;
    line += nl + 1;             /* next element starts after this one's join '\n' */
    total += (size_t) len + 1;  /* element bytes + one join '\n' */
  }
  int nlines = line - 1;

  /* join elements with '\n' */
  char *buf = (char *) R_alloc(total + 1, 1);
  size_t off = 0;
  for (int i = 0; i < n; i++) {
    SEXP s = STRING_ELT(text, i);
    int len = LENGTH(s);
    memcpy(buf + off, CHAR(s), (size_t) len);
    off += (size_t) len;
    if (i < n - 1)
      buf[off++] = '\n';
  }

  /* parse the joined document (sourcepos gives exact code-block line numbers;
   * no extensions are needed, as prose detection only depends on code blocks) */
  SEXP joined = PROTECT(Rf_allocVector(STRSXP, 1));
  SET_STRING_ELT(joined, 0, Rf_mkCharLenCE(buf, (int) off, CE_UTF8));
  cmark_parser *parser;
  cmark_node *doc = parse_document(
    joined, R_NilValue, CMARK_OPT_DEFAULT | CMARK_OPT_SOURCEPOS, &parser
  );

  /* mark every line covered by a code block (1-based; index 0 unused) */
  char *is_code = (char *) R_alloc((size_t) nlines + 1, 1);
  memset(is_code, 0, (size_t) nlines + 1);
  cmark_iter *iter = cmark_iter_new(doc);
  cmark_event_type ev;
  while ((ev = cmark_iter_next(iter)) != CMARK_EVENT_DONE) {
    if (ev != CMARK_EVENT_ENTER) continue;
    cmark_node *node = cmark_iter_get_node(iter);
    if (cmark_node_get_type(node) != CMARK_NODE_CODE_BLOCK) continue;
    int s = cmark_node_get_start_line(node), e = cmark_node_get_end_line(node);
    if (s < 1) s = 1;
    if (e > nlines) e = nlines;
    for (int L = s; L <= e; L++) is_code[L] = 1;
  }
  cmark_iter_free(iter);
  cmark_parser_free(parser);
  cmark_node_free(doc);

  /* an element is prose unless any of its lines is a code line */
  int nprose = 0;
  for (int i = 0; i < n; i++) {
    int s = start_line[i], e = (i < n - 1) ? start_line[i + 1] - 1 : nlines, code = 0;
    for (int L = s; L <= e; L++)
      if (is_code[L]) { code = 1; break; }
    if (!code) nprose++;
  }
  SEXP out = PROTECT(Rf_allocVector(INTSXP, nprose));
  int j = 0;
  for (int i = 0; i < n; i++) {
    int s = start_line[i], e = (i < n - 1) ? start_line[i + 1] - 1 : nlines, code = 0;
    for (int L = s; L <= e; L++)
      if (is_code[L]) { code = 1; break; }
    if (!code) INTEGER(out)[j++] = i + 1;
  }

  UNPROTECT(2);
  return out;
}

/* Convert Pandoc-style attribute list bodies to HTML attribute strings, sharing
 * the C implementation used to render code block attributes (see
 * litedown_render_attrs in extensions/attributes.c). This is the engine behind
 * R's convert_attrs(): `specs` is a character vector where each element is the
 * text inside the braces (e.g. `.r .js #foo k="v"`), already stripped of the
 * curly braces and preprocessed by R (curly-quote / LaTeX-escape normalization).
 * Returns a character vector of the same length, each the assembled attribute
 * string in the canonical order class, id, then key=value tokens. `prefix` is
 * the string prepended to the first class (e.g. "" for headings/links/divs;
 * the code-block render func passes "language-" directly). NA inputs map to NA. */
SEXP R_convert_attrs(SEXP specs, SEXP prefix) {
  if (!Rf_isString(specs))
    Rf_error("Argument 'specs' must be a character vector.");
  if (!Rf_isString(prefix) || Rf_length(prefix) != 1)
    Rf_error("Argument 'prefix' must be a single string.");

  const char *pfx = Rf_translateCharUTF8(STRING_ELT(prefix, 0));
  int n = Rf_length(specs);
  SEXP out = PROTECT(Rf_allocVector(STRSXP, n));
  cmark_mem *mem = cmark_get_default_mem_allocator();

  for (int i = 0; i < n; i++) {
    SEXP s = STRING_ELT(specs, i);
    if (s == NA_STRING) {
      SET_STRING_ELT(out, i, NA_STRING);
      continue;
    }
    {
      const char *d = Rf_translateCharUTF8(s);
      cmark_strbuf buf = CMARK_BUF_INIT(mem);
      litedown_render_attrs(&buf, (const unsigned char *) d,
                            (bufsize_t) strlen(d), pfx);
      cmark_strbuf_putc(&buf, '\0');
      SET_STRING_ELT(out, i, Rf_mkCharCE((const char *) buf.ptr, CE_UTF8));
      cmark_strbuf_free(&buf);
    }
  }

  UNPROTECT(1);
  return out;
}
