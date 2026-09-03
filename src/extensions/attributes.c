/* Pandoc-style code block attributes extension for litedown.
 *
 * Handles fenced code blocks whose info string is a brace-delimited attribute
 * list, e.g.
 *
 *     ```{.r .numberLines #foo style="color: red;"}
 *     1 + 1
 *     ```
 *
 * and renders it in HTML as
 *
 *     <pre><code class="language-r numberLines" id="foo" style="color: red;">
 *
 * i.e. `.class` tokens become a single space-separated `class` attribute (each
 * prefixed with `language-` only for the first one, matching cmark's own
 * `language-` convention and litedown's previous R output), `#id` becomes an
 * `id` attribute, `{-}` is shorthand for `.unnumbered`, and `key=value` /
 * `key="value"` tokens are emitted verbatim as HTML attributes. The class
 * attribute (if any) is emitted first, then id, then the remaining attributes
 * in source order, matching the output litedown used to produce by smuggling
 * the info string past cmark in R (the `id_string()` / `convert_attrs()` dance
 * in R/mark.R and R/utils.R).
 *
 * Only html_render_func is set: for every other output format (latex, xml, man,
 * commonmark, plaintext) the node falls through to cmark's built-in code-block
 * rendering, which ignores the info string for those formats. This matches the
 * old R behavior, where the smuggled attributes were stripped for LaTeX and
 * never reached the other formats.
 *
 * Raw content blocks (`{=html}` / `{=latex}` / `{=tex}`) are owned by the
 * rawblock extension and are deliberately NOT claimed here (a code block can
 * carry only one extension); the postprocess pass skips any info string whose
 * first non-space character after `{` is `=`.
 *
 * The extension does not create a new node type: a postprocess pass sets
 * `node->extension` on the matching CMARK_NODE_CODE_BLOCK nodes so the render
 * dispatch calls back into this file.
 *
 * This is a litedown-only file; it is not part of upstream cmark-gfm and is not
 * touched by src/patches/sync.sh.
 */

#include "attributes.h"
#include <node.h>
#include <render.h>
#include <html.h>
#include <houdini.h>
#include <string.h>

/* Whether `info` is a brace-delimited attribute list this extension handles,
 * i.e. `{...}` (after trimming surrounding spaces) that is not a raw content
 * block `{=...}`. Empty braces `{}` are not claimed (nothing to add). */
static int is_attr_info(cmark_chunk *info) {
  const unsigned char *d = info->data;
  bufsize_t n = info->len;
  bufsize_t i = 0;

  if (!d)
    return 0;
  while (i < n && (d[i] == ' ' || d[i] == '\t'))
    i++;
  while (n > i && (d[n - 1] == ' ' || d[n - 1] == '\t'))
    n--;
  if (n - i < 2 || d[i] != '{' || d[n - 1] != '}')
    return 0;
  if (d[i + 1] == '=')   /* raw content block: owned by the rawblock extension */
    return 0;
  return 1;
}

/* Locate the attribute-list body (the text between the outer braces), trimming
 * surrounding spaces on the whole info string first. Sets *start/*end to the
 * byte range [start, end) of the body. Assumes is_attr_info() returned true. */
static void attr_body(cmark_chunk *info, bufsize_t *start, bufsize_t *end) {
  const unsigned char *d = info->data;
  bufsize_t n = info->len, i = 0;

  while (i < n && (d[i] == ' ' || d[i] == '\t'))
    i++;
  while (n > i && (d[n - 1] == ' ' || d[n - 1] == '\t'))
    n--;
  *start = i + 1;   /* skip '{' */
  *end = n - 1;     /* index of closing '}' */
}

/* Emit a class/id token value, HTML-escaping it (matching cmark's own
 * escape_html for the language class). */
static void put_value(cmark_strbuf *html, const unsigned char *s, bufsize_t len) {
  houdini_escape_html0(html, s, len, 0);
}

/* Render func: build the opening <pre><code ...> tag from the attribute list,
 * then the (escaped) code body, then the closing tags. */
static void html_render(cmark_syntax_extension *extension,
                        cmark_html_renderer *renderer, cmark_node *node,
                        cmark_event_type ev_type, int options) {
  cmark_strbuf *html = renderer->html;
  cmark_chunk *info = &node->as.code.info;
  cmark_chunk *lit = &node->as.code.literal;
  const unsigned char *d = info->data;
  bufsize_t start, end, i;
  int n_class = 0, have_id = 0;
  /* buffer for the remaining (key=value) attributes, emitted after class/id */
  cmark_strbuf rest = CMARK_BUF_INIT(renderer->html->mem);
  (void)extension;

  if (ev_type != CMARK_EVENT_ENTER)
    return;

  attr_body(info, &start, &end);

  cmark_html_render_cr(html);
  cmark_strbuf_puts(html, "<pre");
  cmark_html_render_sourcepos(node, html, options);
  cmark_strbuf_puts(html, "><code");

  /* First pass: emit the class attribute, collecting all .class tokens (and the
   * {-} / .unnumbered shorthand). We scan the whole body, skipping non-class
   * tokens, so classes always come first regardless of source order. */
  i = start;
  while (i < end) {
    bufsize_t tok, tend;
    /* skip spaces */
    while (i < end && (d[i] == ' ' || d[i] == '\t'))
      i++;
    if (i >= end)
      break;
    tok = i;
    /* a token runs to the next space, but a quoted value may contain spaces */
    while (i < end && d[i] != ' ' && d[i] != '\t') {
      if (d[i] == '"') {
        i++;
        while (i < end && d[i] != '"')
          i++;
      }
      i++;
    }
    tend = i;
    if (d[tok] == '.' && tend - tok > 1) {
      if (n_class == 0)
        cmark_strbuf_puts(html, " class=\"language-");
      else
        cmark_strbuf_putc(html, ' ');
      put_value(html, d + tok + 1, tend - tok - 1);
      n_class++;
    } else if (d[tok] == '-' && tend - tok == 1) {
      /* {-} is shorthand for .unnumbered */
      if (n_class == 0)
        cmark_strbuf_puts(html, " class=\"language-");
      else
        cmark_strbuf_putc(html, ' ');
      cmark_strbuf_puts(html, "unnumbered");
      n_class++;
    }
  }
  if (n_class > 0)
    cmark_strbuf_putc(html, '"');

  /* Second pass: id (#id, first one wins) and remaining key=value attributes,
   * in source order, buffered in `rest`. */
  i = start;
  while (i < end) {
    bufsize_t tok, tend;
    while (i < end && (d[i] == ' ' || d[i] == '\t'))
      i++;
    if (i >= end)
      break;
    tok = i;
    while (i < end && d[i] != ' ' && d[i] != '\t') {
      if (d[i] == '"') {
        i++;
        while (i < end && d[i] != '"')
          i++;
      }
      i++;
    }
    tend = i;
    if (d[tok] == '#' && tend - tok > 1) {
      if (!have_id) {
        cmark_strbuf_puts(html, " id=\"");
        put_value(html, d + tok + 1, tend - tok - 1);
        cmark_strbuf_putc(html, '"');
        have_id = 1;
      }
    } else if (d[tok] != '.' && !(d[tok] == '-' && tend - tok == 1)) {
      /* a key=value (or bare key) attribute: emit verbatim, space-separated */
      cmark_strbuf_putc(&rest, ' ');
      cmark_strbuf_put(&rest, d + tok, tend - tok);
    }
  }
  cmark_strbuf_put(html, rest.ptr, rest.size);
  cmark_strbuf_free(&rest);

  cmark_strbuf_putc(html, '>');
  houdini_escape_html0(html, lit->data, lit->len, 0);
  cmark_strbuf_puts(html, "</code></pre>\n");
}

/* Set node->extension on every code block whose info string is an attribute
 * list, so the render dispatch routes it here. */
static cmark_node *postprocess(cmark_syntax_extension *self, cmark_parser *parser,
                               cmark_node *root) {
  cmark_iter *iter = cmark_iter_new(root);
  cmark_event_type ev;
  (void)parser;

  while ((ev = cmark_iter_next(iter)) != CMARK_EVENT_DONE) {
    cmark_node *node = cmark_iter_get_node(iter);
    if (ev != CMARK_EVENT_ENTER || node->type != CMARK_NODE_CODE_BLOCK)
      continue;
    /* skip blocks already claimed by another extension (e.g. rawblock) */
    if (node->extension)
      continue;
    if (is_attr_info(&node->as.code.info))
      cmark_node_set_syntax_extension(node, self);
  }

  cmark_iter_free(iter);
  return root;
}

cmark_syntax_extension *create_attributes_extension(void) {
  cmark_syntax_extension *ext = cmark_syntax_extension_new("attributes");

  cmark_syntax_extension_set_html_render_func(ext, html_render);
  cmark_syntax_extension_set_postprocess_func(ext, postprocess);

  return ext;
}
