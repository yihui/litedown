/* Raw content block extension for litedown.
 *
 * Handles fenced code blocks whose info string is `{=FORMAT}`, e.g.
 *
 *     ```{=html}
 *     <hr class="special">
 *     ```
 *
 * Such a block is emitted verbatim (no HTML/LaTeX escaping) into the matching
 * output format and discarded for all other formats:
 *   {=html}          -> raw in HTML output, dropped elsewhere
 *   {=latex}/{=tex}  -> raw in LaTeX output, dropped elsewhere
 * One documented exception: a raw {=latex}/{=tex} block whose body is entirely
 * LaTeX math environments (\begin{...} ... \end{...}) is also rendered in HTML
 * output, wrapped in <p>...</p>, so the JS math library (KaTeX/MathJax) can
 * typeset it. This mirrors the old post-processing in R/mark.R that inspected
 * `<pre><code class="language-{=...}">` after rendering; moving it here removes
 * that regex round-trip and keeps the code-block info string intact.
 *
 * Only html_render_func and latex_render_func are set: for every other output
 * format (commonmark, plaintext, man, xml) the node falls through to cmark's
 * built-in code-block rendering, matching the previous behavior for those
 * formats (the info string simply appears as the code language).
 *
 * The extension does not create a new node type: a postprocess pass sets
 * `node->extension` on the matching CMARK_NODE_CODE_BLOCK nodes so the render
 * dispatch calls back into this file, and the render funcs read the target from
 * `node->as.code.info` and the body from `node->as.code.literal`.
 *
 * This is a litedown-only file; it is not part of upstream cmark-gfm and is not
 * touched by src/patches/sync.sh.
 */

#include "rawblock.h"
#include "math.h"
#include <parser.h>
#include <node.h>
#include <render.h>
#include <html.h>
#include <string.h>

/* RAW_NONE: not a raw block (ordinary code block, left to cmark).
 * RAW_HTML/RAW_LATEX: known raw target.
 * RAW_OTHER: a `{=...}` block with an unrecognized target (e.g. {=markdown});
 *   claimed by the extension but discarded in every output format, matching the
 *   old R behavior. */
typedef enum { RAW_NONE, RAW_HTML, RAW_LATEX, RAW_OTHER } raw_target;

/* Classify a code block's info string. Recognizes any info of the form
 * `{=TARGET}` as a raw block (leading/trailing spaces around the braces are
 * tolerated, as a block written "``` {=html}" keeps the inner text exactly). */
static raw_target classify(cmark_chunk *info) {
  const unsigned char *d = info->data;
  bufsize_t n = info->len;
  bufsize_t i = 0, j;

  if (!d)
    return RAW_NONE;
  /* trim surrounding spaces */
  while (i < n && (d[i] == ' ' || d[i] == '\t'))
    i++;
  while (n > i && (d[n - 1] == ' ' || d[n - 1] == '\t'))
    n--;
  if (n - i < 3 || d[i] != '{' || d[i + 1] != '=' || d[n - 1] != '}')
    return RAW_NONE;
  i += 2;      /* skip "{=" */
  j = n - 1;   /* index of closing '}' */
  {
    bufsize_t len = j - i;
    if (len == 4 && strncmp((const char *)(d + i), "html", 4) == 0)
      return RAW_HTML;
    if (len == 5 && strncmp((const char *)(d + i), "latex", 5) == 0)
      return RAW_LATEX;
    if (len == 3 && strncmp((const char *)(d + i), "tex", 3) == 0)
      return RAW_LATEX;
  }
  return RAW_OTHER;   /* unknown target (e.g. {=markdown}): discarded everywhere */
}

/* HTML-escape only &, <, > (matching the old R path, which escaped the code
 * block body via cmark's escape_html and then unescaped that same subset). */
static void escape_html_amp(cmark_strbuf *html, const unsigned char *s,
                            bufsize_t len) {
  bufsize_t k;
  for (k = 0; k < len; k++) {
    switch (s[k]) {
    case '&': cmark_strbuf_puts(html, "&amp;"); break;
    case '<': cmark_strbuf_puts(html, "&lt;"); break;
    case '>': cmark_strbuf_puts(html, "&gt;"); break;
    default:  cmark_strbuf_putc(html, s[k]); break;
    }
  }
}

/* Whether a raw {=latex}/{=tex} body is a LaTeX math environment. The body is
 * not NUL-terminated in the node, so copy it out for litedown_is_math_env(). */
static int body_is_math_env(cmark_node *node) {
  cmark_chunk *lit = &node->as.code.literal;
  char *buf;
  int ok;
  if (!lit->data || lit->len == 0)
    return 0;
  buf = (char *)malloc(lit->len + 1);
  if (!buf)
    return 0;
  memcpy(buf, lit->data, lit->len);
  buf[lit->len] = '\0';
  ok = litedown_is_math_env(buf);
  free(buf);
  return ok;
}

static void html_render(cmark_syntax_extension *extension,
                        cmark_html_renderer *renderer, cmark_node *node,
                        cmark_event_type ev_type, int options) {
  cmark_strbuf *html = renderer->html;
  cmark_chunk *lit = &node->as.code.literal;
  raw_target t;
  (void)extension;
  (void)options;
  if (ev_type != CMARK_EVENT_ENTER)
    return;
  t = classify(&node->as.code.info);
  if (t == RAW_HTML) {
    /* raw HTML: emit the body verbatim (it already ends in a newline) */
    cmark_html_render_cr(html);
    cmark_strbuf_put(html, lit->data, lit->len);
  } else if (t == RAW_LATEX && body_is_math_env(node)) {
    /* LaTeX math environment in a raw block: render in HTML too, wrapped in a
     * paragraph, so the JS math library can typeset it (only &,<,> escaped) */
    cmark_html_render_cr(html);
    cmark_strbuf_puts(html, "<p>\n");
    escape_html_amp(html, lit->data, lit->len);
    cmark_strbuf_puts(html, "</p>\n");
  }
  /* other raw targets (e.g. {=latex} non-math, {=markdown}) are discarded */
}

static void latex_render(cmark_syntax_extension *extension,
                         cmark_renderer *renderer, cmark_node *node,
                         cmark_event_type ev_type, int options) {
  cmark_chunk *lit = &node->as.code.literal;
  raw_target t;
  (void)extension;
  (void)options;
  if (ev_type != CMARK_EVENT_ENTER)
    return;
  t = classify(&node->as.code.info);
  if (t != RAW_LATEX)
    return;   /* {=html} and unknown targets are discarded in LaTeX output */
  {
    /* emit the body verbatim, then a blank line to separate it like a block.
     * Trim trailing newlines from the code literal (cmark keeps one) so the
     * blankline() below controls the spacing, matching the old R output. */
    bufsize_t n = lit->len;
    char *buf;
    while (n > 0 && (lit->data[n - 1] == '\n' || lit->data[n - 1] == '\r'))
      n--;
    buf = (char *)malloc(n + 1);
    if (!buf)
      return;
    memcpy(buf, lit->data, n);
    buf[n] = '\0';
    renderer->out(renderer, node, buf, false, LITERAL);
    renderer->blankline(renderer);
    free(buf);
  }
}

/* Set node->extension on every code block that is a raw content block, so the
 * render dispatch routes it to this extension. */
static cmark_node *postprocess(cmark_syntax_extension *self, cmark_parser *parser,
                               cmark_node *root) {
  cmark_iter *iter = cmark_iter_new(root);
  cmark_event_type ev;
  (void)parser;

  while ((ev = cmark_iter_next(iter)) != CMARK_EVENT_DONE) {
    cmark_node *node = cmark_iter_get_node(iter);
    if (ev != CMARK_EVENT_ENTER || node->type != CMARK_NODE_CODE_BLOCK)
      continue;
    if (classify(&node->as.code.info) != RAW_NONE)
      cmark_node_set_syntax_extension(node, self);
  }

  cmark_iter_free(iter);
  return root;
}

cmark_syntax_extension *create_rawblock_extension(void) {
  cmark_syntax_extension *ext = cmark_syntax_extension_new("rawblock");

  cmark_syntax_extension_set_html_render_func(ext, html_render);
  cmark_syntax_extension_set_latex_render_func(ext, latex_render);
  cmark_syntax_extension_set_postprocess_func(ext, postprocess);

  return ext;
}
