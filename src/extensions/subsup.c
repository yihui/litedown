/* Superscript / subscript (and strikethrough) extension for litedown.
 *
 * Recognizes:
 *   ^text^   -> superscript   (<sup>...</sup> / \textsuperscript{...})
 *   ~text~   -> subscript     (<sub>...</sub> / \textsubscript{...})
 *   ~~text~~ -> strikethrough (<del>...</del> / \sout{...})
 *
 * This replaces the token round-trip that R/mark.R used to do for superscript
 * and subscript, and also subsumes GFM's strikethrough: because a single
 * inline delimiter character can be owned by only one extension (see cmark's
 * get_extension_for_special_char()), the extension that claims '~' must handle
 * both the single-tilde subscript and the double-tilde strikethrough. The
 * upstream 'strikethrough' extension is therefore not attached by litedown; the
 * 'subscript' extension here provides the same `~~...~~` behaviour.
 *
 * The content between the delimiters is parsed as ordinary inline Markdown (the
 * delimiters go through cmark's emphasis machinery, exactly like * and _), so
 * nesting works, e.g. `x^*a*^` -> x<sup><em>a</em></sup>. Matching uses GFM
 * flanking rules rather than the old R character-class restriction; this is a
 * documented behaviour change (spaces adjacent to the delimiters are allowed
 * as long as the run is flanking).
 *
 * This is a litedown-only file; it is not part of upstream cmark-gfm and is not
 * touched by src/patches/sync.sh.
 */

#include "subsup.h"
#include <parser.h>
#include <render.h>
#include <string.h>

cmark_node_type CMARK_NODE_SUPERSCRIPT;
cmark_node_type CMARK_NODE_SUBSCRIPT;
cmark_node_type CMARK_NODE_STRIKETHROUGH2;

/* Scan a run of the delimiter character and, if it is flanking, push it onto
 * the delimiter stack so process_emphasis() can pair it later. `allow_len`
 * is a bitmask of run lengths we are willing to treat as a delimiter: bit 1
 * (0x1 << 1) for a single-char run, bit 2 (0x1 << 2) for a two-char run. A run
 * whose length is not allowed is emitted as literal text and never paired. */
static cmark_node *match_delim(cmark_syntax_extension *self, cmark_parser *parser,
                               cmark_inline_parser *inline_parser,
                               unsigned char character, unsigned allow_len) {
  cmark_node *res;
  int left_flanking, right_flanking, punct_before, punct_after, delims;
  char buffer[8];

  delims = cmark_inline_parser_scan_delimiters(
      inline_parser, (int)sizeof(buffer) - 1, character,
      &left_flanking, &right_flanking, &punct_before, &punct_after);

  memset(buffer, character, delims);
  buffer[delims] = 0;

  res = cmark_node_new_with_mem(CMARK_NODE_TEXT, parser->mem);
  cmark_node_set_literal(res, buffer);
  res->start_line = res->end_line = cmark_inline_parser_get_line(inline_parser);
  res->start_column = cmark_inline_parser_get_column(inline_parser) - delims;

  if ((left_flanking || right_flanking) && delims >= 1 && delims <= 2 &&
      (allow_len & (1u << delims)))
    cmark_inline_parser_push_delimiter(inline_parser, character, left_flanking,
                                       right_flanking, res);

  return res;
}

static cmark_node *match_sup(cmark_syntax_extension *self, cmark_parser *parser,
                             cmark_node *parent, unsigned char character,
                             cmark_inline_parser *inline_parser) {
  int start;
  cmark_chunk *chunk;
  if (character != '^')
    return NULL;
  /* Do not treat '^' right after '[' as a superscript delimiter: that is the
   * start of a footnote reference ([^label]), which the core parser handles.
   * Scanning it here would split the "[^label]" text run and corrupt the
   * footnote reference. Leave it as literal text (return NULL without
   * advancing the parser). */
  start = cmark_inline_parser_get_offset(inline_parser);
  chunk = cmark_inline_parser_get_chunk(inline_parser);
  if (start > 0 && chunk->data[start - 1] == '[')
    return NULL;
  return match_delim(self, parser, inline_parser, '^', 1u << 1);
}

static cmark_node *match_sub(cmark_syntax_extension *self, cmark_parser *parser,
                             cmark_node *parent, unsigned char character,
                             cmark_inline_parser *inline_parser) {
  int opts = parser->options;
  unsigned allow_len = 0;
  if (character != '~')
    return NULL;
  /* '~' is owned by this one extension but drives two features: single-tilde
   * subscript and double-tilde strikethrough. Allow only the delimiter lengths
   * whose feature is enabled (subscript -> length 1, strikethrough -> length
   * 2), so the `-subscript` and `-strikethrough` options stay independent. */
  if (opts & LITEDOWN_OPT_SUBSCRIPT)
    allow_len |= (1u << 1);
  if (opts & LITEDOWN_OPT_STRIKETHROUGH)
    allow_len |= (1u << 2);
  if (!allow_len)
    return NULL;
  return match_delim(self, parser, inline_parser, '~', allow_len);
}

/* Pair an opener with a closer: turn the opener's text node into the wrapping
 * node and move everything in between inside it. Mirrors the strikethrough
 * extension's insert(), but chooses the node type from the delimiter run
 * length (1 tilde -> subscript, 2 tildes -> strikethrough; 1 caret ->
 * superscript). */
static delimiter *insert(cmark_syntax_extension *self, cmark_parser *parser,
                         cmark_inline_parser *inline_parser, delimiter *opener,
                         delimiter *closer) {
  cmark_node *wrap;
  cmark_node *tmp, *next;
  delimiter *delim, *tmp_delim;
  delimiter *res = closer->next;
  cmark_node_type type;
  bufsize_t len = opener->inl_text->as.literal.len;

  wrap = opener->inl_text;

  if (opener->inl_text->as.literal.len != closer->inl_text->as.literal.len)
    goto done;

  if (opener->delim_char == '^')
    type = CMARK_NODE_SUPERSCRIPT;
  else
    type = (len == 2) ? CMARK_NODE_STRIKETHROUGH2 : CMARK_NODE_SUBSCRIPT;

  if (!cmark_node_set_type(wrap, type))
    goto done;

  cmark_node_set_syntax_extension(wrap, self);

  tmp = cmark_node_next(opener->inl_text);
  while (tmp) {
    if (tmp == closer->inl_text)
      break;
    next = cmark_node_next(tmp);
    cmark_node_append_child(wrap, tmp);
    tmp = next;
  }

  wrap->end_column =
      closer->inl_text->start_column + closer->inl_text->as.literal.len - 1;
  cmark_node_free(closer->inl_text);

done:
  delim = closer;
  while (delim != NULL && delim != opener) {
    tmp_delim = delim->previous;
    cmark_inline_parser_remove_delimiter(inline_parser, delim);
    delim = tmp_delim;
  }
  cmark_inline_parser_remove_delimiter(inline_parser, opener);

  return res;
}

static const char *get_type_string(cmark_syntax_extension *extension,
                                   cmark_node *node) {
  if (node->type == CMARK_NODE_SUPERSCRIPT)
    return "superscript";
  if (node->type == CMARK_NODE_SUBSCRIPT)
    return "subscript";
  if (node->type == CMARK_NODE_STRIKETHROUGH2)
    return "strikethrough";
  return "<unknown>";
}

static int can_contain(cmark_syntax_extension *extension, cmark_node *node,
                       cmark_node_type child_type) {
  if (node->type != CMARK_NODE_SUPERSCRIPT &&
      node->type != CMARK_NODE_SUBSCRIPT &&
      node->type != CMARK_NODE_STRIKETHROUGH2)
    return false;
  return CMARK_NODE_TYPE_INLINE_P(child_type);
}

static void html_render(cmark_syntax_extension *extension,
                        cmark_html_renderer *renderer, cmark_node *node,
                        cmark_event_type ev_type, int options) {
  bool entering = (ev_type == CMARK_EVENT_ENTER);
  const char *tag;
  if (node->type == CMARK_NODE_SUPERSCRIPT)
    tag = entering ? "<sup>" : "</sup>";
  else if (node->type == CMARK_NODE_SUBSCRIPT)
    tag = entering ? "<sub>" : "</sub>";
  else
    tag = entering ? "<del>" : "</del>";
  cmark_strbuf_puts(renderer->html, tag);
}

static void latex_render(cmark_syntax_extension *extension,
                         cmark_renderer *renderer, cmark_node *node,
                         cmark_event_type ev_type, int options) {
  bool entering = (ev_type == CMARK_EVENT_ENTER);
  const char *open;
  if (node->type == CMARK_NODE_SUPERSCRIPT)
    open = "\\textsuperscript{";
  else if (node->type == CMARK_NODE_SUBSCRIPT)
    open = "\\textsubscript{";
  else
    open = "\\sout{";  /* requires \usepackage{ulem} */
  renderer->out(renderer, node, entering ? open : "}", false, LITERAL);
}

static void commonmark_render(cmark_syntax_extension *extension,
                              cmark_renderer *renderer, cmark_node *node,
                              cmark_event_type ev_type, int options) {
  const char *d;
  if (node->type == CMARK_NODE_SUPERSCRIPT)
    d = "^";
  else if (node->type == CMARK_NODE_SUBSCRIPT)
    d = "~";
  else
    d = "~~";
  renderer->out(renderer, node, d, false, LITERAL);
}

static cmark_syntax_extension *make_extension(const char *name,
                                              unsigned char special,
                                              cmark_match_inline_func match) {
  cmark_syntax_extension *ext = cmark_syntax_extension_new(name);
  cmark_llist *special_chars = NULL;
  cmark_mem *mem = cmark_get_default_mem_allocator();

  cmark_syntax_extension_set_get_type_string_func(ext, get_type_string);
  cmark_syntax_extension_set_can_contain_func(ext, can_contain);
  cmark_syntax_extension_set_html_render_func(ext, html_render);
  cmark_syntax_extension_set_latex_render_func(ext, latex_render);
  cmark_syntax_extension_set_commonmark_render_func(ext, commonmark_render);
  cmark_syntax_extension_set_plaintext_render_func(ext, commonmark_render);
  cmark_syntax_extension_set_match_inline_func(ext, match);
  cmark_syntax_extension_set_inline_from_delim_func(ext, insert);

  special_chars = cmark_llist_append(mem, special_chars, (void *)(size_t)special);
  cmark_syntax_extension_set_special_inline_chars(ext, special_chars);
  cmark_syntax_extension_set_emphasis(ext, 1);

  return ext;
}

cmark_syntax_extension *create_superscript_extension(void) {
  cmark_syntax_extension *ext = make_extension("superscript", '^', match_sup);
  CMARK_NODE_SUPERSCRIPT = cmark_syntax_extension_add_node(1);
  return ext;
}

cmark_syntax_extension *create_subscript_extension(void) {
  cmark_syntax_extension *ext = make_extension("subscript", '~', match_sub);
  CMARK_NODE_SUBSCRIPT = cmark_syntax_extension_add_node(1);
  CMARK_NODE_STRIKETHROUGH2 = cmark_syntax_extension_add_node(1);
  return ext;
}
