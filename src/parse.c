/* Parse markdown to an R representation of the cmark AST.
 *
 * Unlike R_render_markdown() (which renders to a string), R_parse_markdown()
 * returns the parse tree so that R code can walk nodes directly instead of
 * parsing rendered XML/HTML with regular expressions.
 */

#include <Rinternals.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
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

/* Parse `text` into a cmark document, attaching the named `extensions`. The
 * caller must free both the returned document (cmark_node_free) and the parser
 * (cmark_parser_free) it stores via `*out_parser`. */
static cmark_node *parse_document(SEXP text, SEXP extensions, int options,
                                  cmark_parser **out_parser) {
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
  *out_parser = parser;
  return cmark_parser_finish(parser);
}

static int parse_options(SEXP hardbreaks, SEXP smart, SEXP normalize, SEXP footnotes) {
  if (!Rf_isLogical(hardbreaks) || !Rf_isLogical(smart) ||
      !Rf_isLogical(normalize) || !Rf_isLogical(footnotes))
    Rf_error("Arguments 'hardbreaks', 'smart', 'normalize', 'footnotes' must be logical.");
  int options = CMARK_OPT_DEFAULT;
  options += CMARK_OPT_SOURCEPOS;
  options += CMARK_OPT_STRIKETHROUGH_DOUBLE_TILDE;
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

/* --- crack(): assemble the block list in C ------------------------------- *
 * R_crack_blocks() walks the document once and returns the ordered list of
 * blocks that crack() produces, so R no longer needs positional for-loops to
 * assemble them. Two block kinds:
 *
 *   code_chunk: a fenced code block whose info string is `{engine ...}`.
 *     Fields: type, lines (c(start,end)), info (the raw `{...}` string),
 *     engine, code (fence- and prefix-stripped body lines), prefix (leading
 *     indentation / blockquote markers, "" if none), double_brace (TRUE if the
 *     header used `{{...}}`), fence1/fence2 (the raw opening/closing fence
 *     lines, used to reconstruct verbatim fences for double-brace chunks).
 *   text_block: everything else, verbatim source lines.
 *     Fields: type, lines, source (the raw lines joined by "\n").
 *
 * Inline code handling (splitting text_block source into text/code segments)
 * stays in R: it is coupled to litedown's inline syntax rules and chunk-option
 * parser, which we deliberately keep out of C. R still gets the inline code
 * positions from R_code_tokens().
 *
 * A fenced block whose info is NOT `{engine ...}` (e.g. ```` ```python ```` or
 * ```` ```{.python} ````) is not a chunk; its raw lines fall through into the
 * surrounding text block, exactly as before. */

/* Does `info` look like a chunk header, i.e. `{` + engine name (alnum/_)?
 * If so, return the engine name via *engine_beg/*engine_len. */
static int is_chunk_info(const char *info, const char **engine_beg, int *engine_len) {
  if (!info) return 0;
  const char *p = info;
  while (*p == '{') p++;
  if (p == info) return 0;               /* must start with at least one '{' */
  const char *b = p;
  while (*p && (isalnum((unsigned char)*p) || *p == '_')) p++;
  if (p == b) return 0;                  /* need a non-empty engine name */
  *engine_beg = b;
  *engine_len = (int)(p - b);
  return 1;
}

/* An index of line offsets into the source buffer, so we can slice raw lines
 * by 1-based line number the way R's split text vector does. */
typedef struct {
  const char *buf;
  bufsize_t len;
  bufsize_t *off;   /* off[i] = byte offset where line i+1 starts */
  int n;            /* number of lines */
} line_index;

static void line_index_init(line_index *ix, const char *buf, bufsize_t len) {
  int cap = 64, n = 0;
  bufsize_t *off = (bufsize_t *)malloc(cap * sizeof(bufsize_t));
  off[n++] = 0;
  for (bufsize_t i = 0; i < len; i++) {
    if (buf[i] == '\n') {
      if (n >= cap) { cap *= 2; off = (bufsize_t *)realloc(off, cap * sizeof(bufsize_t)); }
      off[n++] = i + 1;
    }
  }
  ix->buf = buf; ix->len = len; ix->off = off; ix->n = n;
}

static void line_index_free(line_index *ix) { free(ix->off); }

/* Return the [start,end) byte range of 1-based line `ln` (without newline). */
static void line_range(line_index *ix, int ln, bufsize_t *beg, bufsize_t *end) {
  bufsize_t b = ix->off[ln - 1];
  bufsize_t e = (ln < ix->n) ? ix->off[ln] - 1 : ix->len;   /* drop '\n' */
  if (e > b && ix->buf[e - 1] == '\r') e--;                 /* drop '\r' */
  *beg = b; *end = e;
}

/* mkChar for a byte range of the source buffer, as UTF-8. */
static SEXP mkchar_range(const char *buf, bufsize_t beg, bufsize_t end) {
  return Rf_mkCharLenCE(buf + beg, (int)(end - beg), CE_UTF8);
}

/* Build a code_chunk block list from a code_block node. */
static SEXP make_code_chunk(line_index *ix, cmark_node *node,
                            const char *info, const char *engine_beg, int engine_len) {
  int sline = cmark_node_get_start_line(node);
  int eline = cmark_node_get_end_line(node);

  /* prefix: the bytes before the fence on the opening line (indentation or
   * blockquote markers such as "> "). start_column is 1-based. */
  int scol = cmark_node_get_start_column(node);
  bufsize_t lb, le; line_range(ix, sline, &lb, &le);
  bufsize_t fence_start = lb + (scol - 1);
  if (fence_start > le) fence_start = le;

  /* double_brace: header used {{...}} */
  int double_brace = (info && info[0] == '{' && info[1] == '{');

  /* code body: cmark's literal is already fence- and prefix-stripped; it has a
   * trailing '\n' we drop, then split into lines. An empty body -> 0 lines. */
  const char *lit = cmark_node_get_literal(node);
  int ncode = 0;
  SEXP code;
  if (lit && lit[0]) {
    size_t L = strlen(lit);
    if (L && lit[L - 1] == '\n') L--;
    /* count lines */
    ncode = (L == 0) ? 0 : 1;
    for (size_t i = 0; i < L; i++) if (lit[i] == '\n') ncode++;
    code = PROTECT(Rf_allocVector(STRSXP, ncode));
    size_t start = 0; int k = 0;
    for (size_t i = 0; i <= L; i++) {
      if (i == L || lit[i] == '\n') {
        SET_STRING_ELT(code, k++, Rf_mkCharLenCE(lit + start, (int)(i - start), CE_UTF8));
        start = i + 1;
      }
    }
  } else {
    code = PROTECT(Rf_allocVector(STRSXP, 0));
  }

  const char *nms[] = {"type", "lines", "info", "engine", "code", "prefix",
                       "double_brace", "fence1", "fence2"};
  int nf = 9;
  SEXP out = PROTECT(Rf_allocVector(VECSXP, nf));
  SEXP names = PROTECT(Rf_allocVector(STRSXP, nf));
  for (int i = 0; i < nf; i++) SET_STRING_ELT(names, i, Rf_mkChar(nms[i]));
  Rf_setAttrib(out, R_NamesSymbol, names);
  UNPROTECT(1);

  SET_VECTOR_ELT(out, 0, Rf_mkString("code_chunk"));
  SEXP lines = Rf_allocVector(INTSXP, 2);
  INTEGER(lines)[0] = sline; INTEGER(lines)[1] = eline;
  SET_VECTOR_ELT(out, 1, lines);
  SET_VECTOR_ELT(out, 2, Rf_ScalarString(Rf_mkCharCE(info, CE_UTF8)));
  SET_VECTOR_ELT(out, 3, Rf_ScalarString(Rf_mkCharLenCE(engine_beg, engine_len, CE_UTF8)));
  SET_VECTOR_ELT(out, 4, code);
  SET_VECTOR_ELT(out, 5, Rf_ScalarString(mkchar_range(ix->buf, lb, fence_start)));
  SET_VECTOR_ELT(out, 6, Rf_ScalarLogical(double_brace));
  /* raw opening / closing fence lines (needed only for double-brace chunks) */
  { bufsize_t b, e; line_range(ix, sline, &b, &e);
    SET_VECTOR_ELT(out, 7, Rf_ScalarString(mkchar_range(ix->buf, b, e))); }
  { bufsize_t b, e; line_range(ix, eline, &b, &e);
    SET_VECTOR_ELT(out, 8, Rf_ScalarString(mkchar_range(ix->buf, b, e))); }

  UNPROTECT(2);  /* code, out */
  return out;
}

/* Build a text_block covering source lines l1..l2 (1-based, inclusive). Only
 * the line range is recorded; crack() slices the raw source lines itself (and
 * splits out inline code) in R. */
static SEXP make_text_block(line_index *ix, int l1, int l2) {
  (void) ix;
  SEXP out = PROTECT(Rf_allocVector(VECSXP, 2));
  SEXP names = PROTECT(Rf_allocVector(STRSXP, 2));
  SET_STRING_ELT(names, 0, Rf_mkChar("type"));
  SET_STRING_ELT(names, 1, Rf_mkChar("lines"));
  Rf_setAttrib(out, R_NamesSymbol, names);
  UNPROTECT(1);
  SET_VECTOR_ELT(out, 0, Rf_mkString("text_block"));
  SEXP lines = Rf_allocVector(INTSXP, 2);
  INTEGER(lines)[0] = l1; INTEGER(lines)[1] = l2;
  SET_VECTOR_ELT(out, 1, lines);
  UNPROTECT(1);
  return out;
}

SEXP R_crack_blocks(SEXP text, SEXP hardbreaks, SEXP smart, SEXP normalize,
                    SEXP footnotes, SEXP extensions) {
  if (!Rf_isString(text))
    Rf_error("Argument 'text' must be string.");

  int options = parse_options(hardbreaks, smart, normalize, footnotes);
  cmark_parser *parser;
  cmark_node *doc = parse_document(text, extensions, options, &parser);

  SEXP input = STRING_ELT(text, 0);
  line_index ix;
  line_index_init(&ix, CHAR(input), LENGTH(input));

  /* Collect blocks into a growable list. */
  int cap = 32, nb = 0;
  SEXP *blocks = (SEXP *)malloc(cap * sizeof(SEXP));
  int nprot = 0;                 /* number of blocks PROTECTed */
  int next_line = 1;             /* next source line not yet emitted */

  cmark_iter *iter = cmark_iter_new(doc);
  cmark_event_type ev;
  while ((ev = cmark_iter_next(iter)) != CMARK_EVENT_DONE) {
    if (ev != CMARK_EVENT_ENTER) continue;
    cmark_node *node = cmark_iter_get_node(iter);
    if (cmark_node_get_type(node) != CMARK_NODE_CODE_BLOCK) continue;
    const char *info = cmark_node_get_fence_info(node);
    const char *eng_beg; int eng_len;
    if (!is_chunk_info(info, &eng_beg, &eng_len)) continue;   /* not a chunk */

    int sline = cmark_node_get_start_line(node);
    int eline = cmark_node_get_end_line(node);
    if (sline > next_line) {
      if (nb >= cap) { cap *= 2; blocks = (SEXP *)realloc(blocks, cap * sizeof(SEXP)); }
      blocks[nb++] = PROTECT(make_text_block(&ix, next_line, sline - 1)); nprot++;
    }
    if (nb >= cap) { cap *= 2; blocks = (SEXP *)realloc(blocks, cap * sizeof(SEXP)); }
    blocks[nb++] = PROTECT(make_code_chunk(&ix, node, info, eng_beg, eng_len)); nprot++;
    next_line = eline + 1;
  }
  cmark_iter_free(iter);

  if (next_line <= ix.n) {
    if (nb >= cap) { cap *= 2; blocks = (SEXP *)realloc(blocks, cap * sizeof(SEXP)); }
    blocks[nb++] = PROTECT(make_text_block(&ix, next_line, ix.n)); nprot++;
  }

  SEXP res = PROTECT(Rf_allocVector(VECSXP, nb));
  for (int i = 0; i < nb; i++) SET_VECTOR_ELT(res, i, blocks[i]);

  UNPROTECT(1 + nprot);
  free(blocks);
  line_index_free(&ix);
  cmark_parser_free(parser);
  cmark_node_free(doc);
  return res;
}
