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

/* Advance past one whitespace-separated token in d[*i, len), treating a
 * double-quoted run as part of the token (so a quoted value may contain
 * spaces). On return *tok/*tend bound the token and *i points past it; returns
 * 0 when no token remains. */
static int next_token(const unsigned char *d, bufsize_t len, bufsize_t *i,
                      bufsize_t *tok, bufsize_t *tend) {
  bufsize_t k = *i;
  while (k < len && (d[k] == ' ' || d[k] == '\t'))
    k++;
  if (k >= len)
    return 0;
  *tok = k;
  while (k < len && d[k] != ' ' && d[k] != '\t') {
    if (d[k] == '"') {
      k++;
      while (k < len && d[k] != '"')
        k++;
    }
    k++;
  }
  *tend = k;
  *i = k;
  return 1;
}

/* Build an HTML attribute string from a Pandoc-style attribute list body
 * d[0, len) (the text inside the braces, e.g. `.r .js #foo k="v"`), appending
 * to `out`. Attributes are emitted in the canonical order class, id, then the
 * remaining key=value tokens in source order, each separated from the previous
 * content by a single space (including from any content already in `out`):
 *
 *   .class tokens (and the `{-}`/`.unnumbered` shorthand) -> one class="..."
 *     attribute, each class prefixed by `class_prefix` on the FIRST class only
 *     (pass "language-" for code blocks, "" elsewhere);
 *   #id (first one wins)                                  -> id="...";
 *   key=value (or bare key)                               -> emitted verbatim.
 *
 * This is the single source of truth shared by the code-block render func here
 * and R's convert_attrs() (via the R_convert_attrs binding in parse.c). */
void litedown_render_attrs(cmark_strbuf *out, const unsigned char *d,
                           bufsize_t len, const char *class_prefix) {
  bufsize_t i, tok, tend;
  int n_class = 0;

  /* class attribute: all .class tokens (and {-}/.unnumbered), in source order */
  for (i = 0; next_token(d, len, &i, &tok, &tend);) {
    int is_class = d[tok] == '.' && tend - tok > 1;
    int is_dash = d[tok] == '-' && tend - tok == 1;
    if (!is_class && !is_dash)
      continue;
    if (n_class == 0) {
      if (out->size)
        cmark_strbuf_putc(out, ' ');
      cmark_strbuf_puts(out, "class=\"");
      cmark_strbuf_puts(out, class_prefix);
    } else {
      cmark_strbuf_putc(out, ' ');
    }
    if (is_dash)
      cmark_strbuf_puts(out, "unnumbered");
    else
      houdini_escape_html0(out, d + tok + 1, tend - tok - 1, 0);
    n_class++;
  }
  if (n_class > 0)
    cmark_strbuf_putc(out, '"');

  /* id attribute (first #id wins) */
  for (i = 0; next_token(d, len, &i, &tok, &tend);) {
    if (d[tok] == '#' && tend - tok > 1) {
      if (out->size)
        cmark_strbuf_putc(out, ' ');
      cmark_strbuf_puts(out, "id=\"");
      houdini_escape_html0(out, d + tok + 1, tend - tok - 1, 0);
      cmark_strbuf_putc(out, '"');
      break;
    }
  }

  /* remaining key=value (or bare key) attributes, verbatim, in source order */
  for (i = 0; next_token(d, len, &i, &tok, &tend);) {
    if (d[tok] == '.' || d[tok] == '#' ||
        (d[tok] == '-' && tend - tok == 1))
      continue;
    if (out->size)
      cmark_strbuf_putc(out, ' ');
    cmark_strbuf_put(out, d + tok, tend - tok);
  }
}

/* Render func: build the opening <pre><code ...> tag from the attribute list,
 * then the (escaped) code body, then the closing tags. */
static void html_render(cmark_syntax_extension *extension,
                        cmark_html_renderer *renderer, cmark_node *node,
                        cmark_event_type ev_type, int options) {
  cmark_strbuf *html = renderer->html;
  cmark_chunk *info = &node->as.code.info;
  cmark_chunk *lit = &node->as.code.literal;
  cmark_strbuf attrs = CMARK_BUF_INIT(html->mem);
  bufsize_t start, end;
  (void)extension;

  if (ev_type != CMARK_EVENT_ENTER)
    return;

  attr_body(info, &start, &end);
  litedown_render_attrs(&attrs, info->data + start, end - start, "language-");

  cmark_html_render_cr(html);
  cmark_strbuf_puts(html, "<pre");
  cmark_html_render_sourcepos(node, html, options);
  cmark_strbuf_puts(html, "><code");
  if (attrs.size) {
    cmark_strbuf_putc(html, ' ');
    cmark_strbuf_put(html, attrs.ptr, attrs.size);
  }
  cmark_strbuf_free(&attrs);

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
