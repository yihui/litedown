/* LaTeX math extension for litedown.
 *
 * Recognizes inline math `$...$` and display math `$$...$$` and emits math
 * nodes that render verbatim (only &, <, > are HTML-escaped; nothing is escaped
 * for LaTeX). This replaces the fragile token round-trip that R/mark.R used to
 * hide math from the renderer (see yihui/litedown#33): math is now a real node
 * in the parse tree instead of a placeholder string.
 *
 * The matching rules mirror xfun::protect_math()/escape_math() so that output
 * is unchanged from the old R implementation:
 *   inline  `$x$`   : opening `$` preceded by start-of-text, whitespace or '(',
 *                     and not followed by a space or backtick; closing `$` not
 *                     preceded by a space or backtick, and not followed by
 *                     another `$` or a digit.
 *   display `$$x$$` : opening `$$` preceded as above and not followed by a
 *                     space; closing `$$` not preceded by a space.
 * In both cases the body may not contain a literal `$`.
 *
 * This is a litedown-only file; it is not part of upstream cmark-gfm and is not
 * touched by src/patches/sync.sh.
 */

#include "math.h"
#include <parser.h>
#include <render.h>
#include <html.h>
#include <string.h>

cmark_node_type CMARK_NODE_MATH_INLINE;
cmark_node_type CMARK_NODE_MATH_DISPLAY;
cmark_node_type CMARK_NODE_MATH_ENV;

/* A `$` may open math only at the start of the text run or right after a space
 * or '(' (matching xfun's (?<=^|[\s(]) lookbehind). */
static int can_open_before(unsigned char c) {
  return c == 0 || c == ' ' || c == '\t' || c == '\n' || c == '\r' || c == '(';
}

static cmark_node *match(cmark_syntax_extension *self, cmark_parser *parser,
                         cmark_node *parent, unsigned char character,
                         cmark_inline_parser *inline_parser) {
  cmark_chunk *chunk;
  unsigned char *data;
  bufsize_t len;
  int start, open_len, i, start_col;
  int display;
  cmark_node *node;
  cmark_node_type type;

  if (character != '$')
    return NULL;

  start = cmark_inline_parser_get_offset(inline_parser);
  start_col = cmark_inline_parser_get_column(inline_parser);
  chunk = cmark_inline_parser_get_chunk(inline_parser);
  data = chunk->data;
  len = chunk->len;

  /* opening context: preceded by start / whitespace / '(' */
  if (!can_open_before(start > 0 ? data[start - 1] : 0))
    return NULL;

  display = (start + 1 < len && data[start + 1] == '$');
  open_len = display ? 2 : 1;

  /* need at least one body char before a possible closer */
  if (start + open_len >= len)
    return NULL;

  /* opening `$`/`$$` must not be followed by a space (and, for inline, not by a
   * backtick either) */
  {
    unsigned char after = data[start + open_len];
    if (after == ' ' || after == '\t' || (!display && after == '`'))
      return NULL;
  }

  /* scan the body: everything up to the next `$` (body may not contain `$`) */
  i = start + open_len;
  while (i < len && data[i] != '$')
    i++;
  if (i >= len)
    return NULL;                 /* no closing `$` */
  if (i == start + open_len)
    return NULL;                 /* empty body */

  /* closing `$`/`$$` must not be preceded by a space (nor, inline, a backtick) */
  {
    unsigned char before = data[i - 1];
    if (before == ' ' || before == '\t' || (!display && before == '`'))
      return NULL;
  }

  if (display) {
    if (!(i + 1 < len && data[i + 1] == '$'))
      return NULL;               /* single `$` cannot close display math */
  } else {
    /* inline closing `$` must not be followed by another `$` or a digit */
    unsigned char after = (i + 1 < len) ? data[i + 1] : 0;
    if (after == '$' || (after >= '0' && after <= '9'))
      return NULL;
  }

  /* consume through the closer */
  cmark_inline_parser_set_offset(inline_parser, i + open_len);

  type = display ? CMARK_NODE_MATH_DISPLAY : CMARK_NODE_MATH_INLINE;
  node = cmark_node_new_with_mem(type, parser->mem);
  cmark_node_set_syntax_extension(node, self);
  /* store the body (without the delimiters) as the node literal. Custom node
   * types are not handled by cmark_node_set_literal(), so build as.literal
   * directly (the strikethrough extension reads as.literal the same way). The
   * body is not NUL-terminated in the source chunk, so copy it out. */
  {
    bufsize_t blen = i - start - open_len;
    unsigned char *body = (unsigned char *)parser->mem->calloc(blen + 1, 1);
    memcpy(body, data + start + open_len, blen);
    node->as.literal.data = body;
    node->as.literal.len = blen;
    node->as.literal.alloc = 1;
  }
  node->start_line = node->end_line = cmark_inline_parser_get_line(inline_parser);
  node->start_column = start_col;
  node->end_column = start_col + (i + open_len - start) - 1;
  return node;
}

/* Free the math body. cmark's free_node_as() only frees as.literal for its
 * built-in node types; our custom math node types fall through its default
 * case, so we must release as.literal.data ourselves. cmark calls this
 * opaque_free_func before free_node_as(); as.literal.data aliases as.opaque in
 * the node union, so the non-NULL as.opaque check in the library fires whenever
 * a body was stored. */
static void opaque_free(cmark_syntax_extension *extension, cmark_mem *mem,
                        cmark_node *node) {
  if (node->as.literal.data) {
    mem->free(node->as.literal.data);
    node->as.literal.data = NULL;
    node->as.literal.len = 0;
    node->as.literal.alloc = 0;
  }
}

static const char *get_type_string(cmark_syntax_extension *extension,
                                   cmark_node *node) {
  if (node->type == CMARK_NODE_MATH_INLINE)
    return "math_inline";
  if (node->type == CMARK_NODE_MATH_DISPLAY)
    return "math_display";
  if (node->type == CMARK_NODE_MATH_ENV)
    return "math_env";
  return "<unknown>";
}

/* HTML-escape only &, <, > (matching the old R path, which escaped math bodies
 * with xfun::html_escape's &<> subset rather than cmark's full entity set). */
static void escape_math_html(cmark_strbuf *html, const char *s) {
  const unsigned char *p = (const unsigned char *)s;
  for (; *p; p++) {
    switch (*p) {
    case '&': cmark_strbuf_puts(html, "&amp;"); break;
    case '<': cmark_strbuf_puts(html, "&lt;"); break;
    case '>': cmark_strbuf_puts(html, "&gt;"); break;
    default:  cmark_strbuf_putc(html, *p); break;
    }
  }
}

static void html_render(cmark_syntax_extension *extension,
                        cmark_html_renderer *renderer, cmark_node *node,
                        cmark_event_type ev_type, int options) {
  const char *body = (const char *)node->as.literal.data;
  if (ev_type != CMARK_EVENT_ENTER)
    return;
  if (node->type == CMARK_NODE_MATH_ENV) {
    /* environments render inside their own <p>, verbatim (matching the old R
     * path, which wrapped a raw ```{=tex} math block in <p>...</p>) */
    cmark_html_render_cr(renderer->html);
    cmark_strbuf_puts(renderer->html, "<p>");
    escape_math_html(renderer->html, body ? body : "");
    cmark_strbuf_puts(renderer->html, "</p>\n");
  } else if (node->type == CMARK_NODE_MATH_DISPLAY) {
    cmark_strbuf_puts(renderer->html, "$$");
    escape_math_html(renderer->html, body ? body : "");
    cmark_strbuf_puts(renderer->html, "$$");
  } else {
    cmark_strbuf_puts(renderer->html, "\\(");
    escape_math_html(renderer->html, body ? body : "");
    cmark_strbuf_puts(renderer->html, "\\)");
  }
}

static void latex_render(cmark_syntax_extension *extension,
                         cmark_renderer *renderer, cmark_node *node,
                         cmark_event_type ev_type, int options) {
  const char *body = (const char *)node->as.literal.data;
  if (ev_type != CMARK_EVENT_ENTER)
    return;
  if (node->type == CMARK_NODE_MATH_ENV) {
    /* raw LaTeX environment: emit verbatim, then a blank line like a paragraph */
    renderer->out(renderer, node, body ? body : "", false, LITERAL);
    renderer->blankline(renderer);
  } else if (node->type == CMARK_NODE_MATH_DISPLAY) {
    renderer->out(renderer, node, "$$", false, LITERAL);
    renderer->out(renderer, node, body ? body : "", false, LITERAL);
    renderer->out(renderer, node, "$$", false, LITERAL);
  } else {
    renderer->out(renderer, node, "\\(", false, LITERAL);
    renderer->out(renderer, node, body ? body : "", false, LITERAL);
    renderer->out(renderer, node, "\\)", false, LITERAL);
  }
}

static void plaintext_render(cmark_syntax_extension *extension,
                             cmark_renderer *renderer, cmark_node *node,
                             cmark_event_type ev_type, int options) {
  const char *body = (const char *)node->as.literal.data;
  const char *d = node->type == CMARK_NODE_MATH_DISPLAY ? "$$" : "$";
  if (ev_type != CMARK_EVENT_ENTER)
    return;
  if (node->type == CMARK_NODE_MATH_ENV) {
    renderer->out(renderer, node, body ? body : "", false, LITERAL);
    renderer->blankline(renderer);
    return;
  }
  renderer->out(renderer, node, d, false, LITERAL);
  renderer->out(renderer, node, body ? body : "", false, LITERAL);
  renderer->out(renderer, node, d, false, LITERAL);
}

/* Test whether the raw text of a paragraph is composed entirely of top-level
 * LaTeX environments (\begin{...} ... \end{...}), matching the rule that
 * xfun::escape_math() applies to whole-line \begin/\end pairs. `s` is the
 * paragraph's raw string content (softbreaks are literal '\n'). Returns 1 if
 * every line participates in a balanced sequence of environments with no stray
 * text at depth 0. */
static int is_math_env(const char *s) {
  const char *p = s;
  int depth = 0;
  int saw_env = 0;

  while (*p) {
    const char *line = p;
    const char *eol = line;
    while (*eol && *eol != '\n')
      eol++;
    /* trim trailing spaces/CR from the logical line */
    const char *lend = eol;
    while (lend > line && (lend[-1] == ' ' || lend[-1] == '\t' || lend[-1] == '\r'))
      lend--;
    /* a line must be exactly \begin{name} or \end{name} when at depth 0;
     * inside an environment (depth > 0) any content is allowed */
    if (depth == 0) {
      if (line == lend)
        return 0;               /* blank line at depth 0: not a pure env block */
      if (!(lend - line >= 8 && strncmp(line, "\\begin{", 7) == 0 &&
            lend[-1] == '}'))
        return 0;               /* stray text at depth 0 */
    }
    /* scan the line for \begin{ / \end{ to update depth */
    {
      const char *q = line;
      while (q < lend) {
        if (q[0] == '\\' && strncmp(q, "\\begin{", 7) == 0) {
          depth++; saw_env = 1; q += 7;
        } else if (q[0] == '\\' && strncmp(q, "\\end{", 5) == 0) {
          depth--;
          if (depth < 0)
            return 0;
          q += 5;
        } else {
          q++;
        }
      }
    }
    if (!*eol)
      break;
    p = eol + 1;
  }

  return saw_env && depth == 0;
}

/* Concatenate the raw string content of a paragraph node. cmark keeps the
 * paragraph's `content` strbuf intact through inline parsing, so it still holds
 * the original multi-line source (with '\n' separators and no inline mangling
 * such as _emphasis_ or \\ collapsing). */
static cmark_node *postprocess(cmark_syntax_extension *self, cmark_parser *parser,
                               cmark_node *root) {
  cmark_iter *iter = cmark_iter_new(root);
  cmark_event_type ev;

  while ((ev = cmark_iter_next(iter)) != CMARK_EVENT_DONE) {
    cmark_node *node = cmark_iter_get_node(iter);
    if (ev != CMARK_EVENT_ENTER || node->type != CMARK_NODE_PARAGRAPH)
      continue;
    {
      const char *raw = (const char *)node->content.ptr;
      bufsize_t rlen = node->content.size;
      char *buf;
      /* trim a single trailing newline that cmark appends to paragraph content */
      while (rlen > 0 && (raw[rlen - 1] == '\n' || raw[rlen - 1] == '\r'))
        rlen--;
      if (rlen <= 0)
        continue;
      buf = (char *)parser->mem->calloc(rlen + 1, 1);
      memcpy(buf, raw, rlen);
      if (is_math_env(buf)) {
        cmark_node *m = cmark_node_new_with_mem(CMARK_NODE_MATH_ENV, parser->mem);
        cmark_node_set_syntax_extension(m, self);
        m->start_line = node->start_line;
        m->end_line = node->end_line;
        m->start_column = node->start_column;
        m->end_column = node->end_column;
        m->as.literal.data = (unsigned char *)buf;
        m->as.literal.len = rlen;
        m->as.literal.alloc = 1;
        /* Link m into the tree first so it has valid next/parent pointers, then
         * point the iterator at m's EXIT event (as if we had just finished
         * walking it) before freeing the old paragraph and its children. This
         * keeps the iterator from dereferencing the freed subtree. */
        cmark_node_replace(node, m);
        cmark_iter_reset(iter, m, CMARK_EVENT_EXIT);
        cmark_node_free(node);
      } else {
        parser->mem->free(buf);
      }
    }
  }

  cmark_iter_free(iter);
  return root;
}

cmark_syntax_extension *create_math_extension(void) {
  cmark_syntax_extension *ext = cmark_syntax_extension_new("math");
  cmark_llist *special_chars = NULL;
  cmark_mem *mem = cmark_get_default_mem_allocator();

  cmark_syntax_extension_set_get_type_string_func(ext, get_type_string);
  cmark_syntax_extension_set_opaque_free_func(ext, opaque_free);
  cmark_syntax_extension_set_html_render_func(ext, html_render);
  cmark_syntax_extension_set_latex_render_func(ext, latex_render);
  cmark_syntax_extension_set_commonmark_render_func(ext, plaintext_render);
  cmark_syntax_extension_set_plaintext_render_func(ext, plaintext_render);
  cmark_syntax_extension_set_man_render_func(ext, plaintext_render);

  CMARK_NODE_MATH_INLINE = cmark_syntax_extension_add_node(1);
  CMARK_NODE_MATH_DISPLAY = cmark_syntax_extension_add_node(1);
  CMARK_NODE_MATH_ENV = cmark_syntax_extension_add_node(0);

  cmark_syntax_extension_set_match_inline_func(ext, match);
  cmark_syntax_extension_set_postprocess_func(ext, postprocess);

  special_chars = cmark_llist_append(mem, special_chars, (void *)'$');
  cmark_syntax_extension_set_special_inline_chars(ext, special_chars);

  return ext;
}
