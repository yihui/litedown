/* Pandoc-style inline attributes extension for litedown.
 *
 * Attaches a trailing brace-delimited attribute list to the immediately
 * preceding link or image, e.g.
 *
 *     [text](url){#id .cls key="val"}     ->  <a href="url" class="cls" id="id" key="val">text</a>
 *     ![alt](img){.round width="40%"}     ->  <img src="img" alt="alt" class="round" width="40%" />
 *     [text](){.cls}                      ->  <span class="cls">text</span>
 *
 * The core cmark parser has no notion of inline attributes: it parses the link
 * or image as usual and leaves the following `{...}` as a plain text node. This
 * extension therefore runs as a postprocess pass over the parsed AST: for every
 * LINK / IMAGE node whose next sibling is a text node beginning with a balanced
 * `{...}`, it strips that brace list off the text node, stashes the attribute
 * body (and, for images, the plain-text alt) on the node via user_data, and
 * sets node->extension so the render dispatch calls back into this file.
 *
 * This replaces the post-render string surgery R/mark.R + R/utils.R used to do
 * (move_attrs()/convert_attrs() for images and links, and the
 * `<a href="">` -> `<span>` gsub). Block-level attributes (headings, fenced
 * Divs) are still handled in R.
 *
 * HTML output reuses litedown_render_attrs() (shared with the code-block
 * 'attributes' extension) so the attribute ordering/escaping is identical:
 * class first, then id, then key=value tokens in source order. An empty link
 * href (`[text](){...}`) becomes a <span> instead of an <a>.
 *
 * LaTeX output supports image width (percent -> \linewidth), renders an empty
 * href as a bare group `{...}`, and otherwise emits the same \href / \hyperlink
 * the core renderer would, minus the stray brace text (which the old R code left
 * behind as `\{...\}`). All other attributes are dropped for LaTeX, matching the
 * previous behaviour.
 *
 * This is a litedown-only file; it is not part of upstream cmark-gfm and is not
 * touched by src/patches/sync.sh.
 */

#include "inlineattrs.h"
#include "attributes.h"
#include <node.h>
#include <render.h>
#include <html.h>
#include <houdini.h>
#include <scanners.h>
#include <string.h>
#include <stdlib.h>

/* Data stashed on a claimed node. `info` is the attribute-list body (the text
 * between the outer braces, e.g. `#id .cls key="val"`). `alt` is the plain-text
 * alt of an image (NULL for links); images are stripped of their child nodes so
 * the alt must be captured here for the HTML renderer. */
typedef struct {
  char *info;
  char *alt;
} inlineattrs_data;

static void free_data(cmark_mem *mem, void *user_data) {
  inlineattrs_data *d = (inlineattrs_data *)user_data;
  (void)mem;
  if (!d)
    return;
  free(d->info);
  free(d->alt);
  free(d);
}

/* If `s[0]` is '{', return the index just past the matching '}', treating a
 * double-quoted run as opaque (so a value may contain '}'). Return 0 if `s`
 * does not start with a balanced brace group. */
static bufsize_t brace_len(const unsigned char *s, bufsize_t len) {
  bufsize_t i = 1;
  if (len < 2 || s[0] != '{')
    return 0;
  while (i < len) {
    if (s[i] == '"') {
      i++;
      while (i < len && s[i] != '"')
        i++;
    } else if (s[i] == '}') {
      return i + 1;
    }
    i++;
  }
  return 0;
}

/* Append the plain text of an inline subtree to `buf` (text/code literals
 * verbatim, soft/hard breaks as a single space), mirroring how the core HTML
 * renderer flattens image children into the alt attribute. */
static void collect_alt(cmark_node *node, cmark_strbuf *buf) {
  cmark_node *child;
  for (child = node->first_child; child; child = child->next) {
    switch (child->type) {
    case CMARK_NODE_TEXT:
    case CMARK_NODE_CODE:
    case CMARK_NODE_HTML_INLINE:
      cmark_strbuf_put(buf, child->as.literal.data, child->as.literal.len);
      break;
    case CMARK_NODE_SOFTBREAK:
    case CMARK_NODE_LINEBREAK:
      cmark_strbuf_putc(buf, ' ');
      break;
    default:
      collect_alt(child, buf);
      break;
    }
  }
}

/* Claim `node` (a LINK or IMAGE): stash the attribute body `d[0, blen)` (the
 * text between braces) and, for images, the flattened alt text; then strip the
 * braces off the trailing text node `text` (freeing it if nothing remains) and
 * route the node to this extension's render funcs. */
static void claim(cmark_syntax_extension *self, cmark_node *node,
                  cmark_node *text, const unsigned char *body, bufsize_t blen) {
  inlineattrs_data *data = (inlineattrs_data *)calloc(1, sizeof(*data));
  bufsize_t consumed;

  /* Copy the attribute body, normalizing smart-quoted values: the `smart`
   * option curls the straight double quotes in the prose text stream (and the
   * `{...}` lives in that stream) into U+201C/U+201D before this postprocess
   * runs, so `key="v"` arrives as `key=<U+201C>v<U+201D>`. Fold both back to a
   * straight '"' so the attribute parser and width detection work, matching
   * what R's convert_attrs() used to do. */
  {
    bufsize_t bi, di = 0;
    data->info = (char *)malloc(blen + 1);
    for (bi = 0; bi < blen; bi++) {
      if (bi + 2 < blen && body[bi] == 0xE2 && body[bi + 1] == 0x80 &&
          (body[bi + 2] == 0x9C || body[bi + 2] == 0x9D)) {
        data->info[di++] = '"';
        bi += 2;
      } else {
        data->info[di++] = (char)body[bi];
      }
    }
    data->info[di] = 0;
  }

  if (node->type == CMARK_NODE_IMAGE) {
    cmark_strbuf alt = CMARK_BUF_INIT(cmark_node_mem(node));
    cmark_node *child, *next;
    collect_alt(node, &alt);
    data->alt = (char *)malloc(alt.size + 1);
    memcpy(data->alt, alt.ptr, alt.size);
    data->alt[alt.size] = 0;
    cmark_strbuf_free(&alt);
    /* drop the alt children: the alt is now in user_data, and keeping them
     * would make the LaTeX renderer emit the alt after \includegraphics{} */
    for (child = node->first_child; child; child = next) {
      next = child->next;
      cmark_node_unlink(child);
      cmark_node_free(child);
    }
  }

  cmark_node_set_user_data(node, data);
  cmark_node_set_user_data_free_func(node, free_data);
  cmark_node_set_syntax_extension(node, self);

  /* strip the leading `{...}` from the text node: `body` points just past the
   * opening brace and spans `blen` bytes, so the whole group is the opening
   * brace + body + closing brace. */
  consumed = (bufsize_t)((body - text->as.literal.data) + blen + 1);
  if (consumed >= text->as.literal.len) {
    cmark_node_unlink(text);
    cmark_node_free(text);
  } else {
    bufsize_t rest_len = text->as.literal.len - consumed;
    char *tmp = (char *)malloc(rest_len + 1);
    memcpy(tmp, text->as.literal.data + consumed, rest_len);
    tmp[rest_len] = 0;
    cmark_node_set_literal(text, tmp);
    free(tmp);
  }
}

/* Postprocess: claim every LINK / IMAGE immediately followed by a text node
 * that starts with a balanced `{...}` brace group.
 *
 * claim() mutates the tree (frees the trailing text sibling and an image's
 * children), which would invalidate the node the iterator has cached as its
 * next step. So the walk only records the matches; the mutation happens in a
 * second pass after the iterator is freed. Recorded nodes are chained through a
 * scratch field: each claimed node's user_data temporarily points at the next
 * one (claim() overwrites user_data with the real payload). */
static cmark_node *postprocess(cmark_syntax_extension *self, cmark_parser *parser,
                               cmark_node *root) {
  cmark_iter *iter = cmark_iter_new(root);
  cmark_event_type ev;
  cmark_node *head = NULL;
  (void)parser;

  while ((ev = cmark_iter_next(iter)) != CMARK_EVENT_DONE) {
    cmark_node *node = cmark_iter_get_node(iter);
    cmark_node *text;
    bufsize_t blen;
    if (ev != CMARK_EVENT_ENTER)
      continue;
    if (node->type != CMARK_NODE_LINK && node->type != CMARK_NODE_IMAGE)
      continue;
    if (node->extension)
      continue;
    text = node->next;
    if (!text || text->type != CMARK_NODE_TEXT)
      continue;
    blen = brace_len(text->as.literal.data, text->as.literal.len);
    if (blen < 2)
      continue;
    cmark_node_set_user_data(node, head);
    head = node;
  }
  cmark_iter_free(iter);

  while (head) {
    cmark_node *node = head, *text = node->next;
    bufsize_t blen = brace_len(text->as.literal.data, text->as.literal.len);
    head = (cmark_node *)cmark_node_get_user_data(node);
    cmark_node_set_user_data(node, NULL);
    if (blen < 2)
      continue; /* unreachable: the first pass already checked this */
    /* body excludes the surrounding braces */
    claim(self, node, text, text->as.literal.data + 1, blen - 2);
  }

  return root;
}

static void html_render(cmark_syntax_extension *extension,
                        cmark_html_renderer *renderer, cmark_node *node,
                        cmark_event_type ev_type, int options) {
  cmark_strbuf *html = renderer->html;
  bool entering = (ev_type == CMARK_EVENT_ENTER);
  inlineattrs_data *data = (inlineattrs_data *)cmark_node_get_user_data(node);
  cmark_strbuf attrs = CMARK_BUF_INIT(html->mem);
  const unsigned char *url = node->as.link.url.data;
  bufsize_t url_len = node->as.link.url.len;
  (void)extension;

  if (node->type == CMARK_NODE_IMAGE) {
    if (!entering)
      return;
    cmark_strbuf_puts(html, "<img src=\"");
    if ((options & CMARK_OPT_UNSAFE) || !(scan_dangerous_url(&node->as.link.url, 0)))
      houdini_escape_href(html, url, url_len);
    cmark_strbuf_puts(html, "\" alt=\"");
    if (data->alt)
      houdini_escape_html0(html, (const unsigned char *)data->alt,
                           (bufsize_t)strlen(data->alt), 0);
    litedown_render_attrs(&attrs, (const unsigned char *)data->info,
                          (bufsize_t)strlen(data->info), "");
    cmark_strbuf_puts(html, "\"");
    if (node->as.link.title.len) {
      cmark_strbuf_puts(html, " title=\"");
      houdini_escape_html0(html, node->as.link.title.data,
                           node->as.link.title.len, 0);
      cmark_strbuf_puts(html, "\"");
    }
    if (attrs.size) {
      cmark_strbuf_putc(html, ' ');
      cmark_strbuf_put(html, attrs.ptr, attrs.size);
    }
    cmark_strbuf_free(&attrs);
    cmark_strbuf_puts(html, " />");
    return;
  }

  /* LINK: an empty href becomes a <span>, otherwise an <a href="..."> */
  litedown_render_attrs(&attrs, (const unsigned char *)data->info,
                        (bufsize_t)strlen(data->info), "");
  if (url_len == 0) {
    if (entering) {
      cmark_strbuf_puts(html, "<span");
      if (attrs.size) {
        cmark_strbuf_putc(html, ' ');
        cmark_strbuf_put(html, attrs.ptr, attrs.size);
      }
      cmark_strbuf_puts(html, ">");
    } else {
      cmark_strbuf_puts(html, "</span>");
    }
  } else {
    if (entering) {
      cmark_strbuf_puts(html, "<a href=\"");
      if ((options & CMARK_OPT_UNSAFE) || !(scan_dangerous_url(&node->as.link.url, 0)))
        houdini_escape_href(html, url, url_len);
      cmark_strbuf_puts(html, "\"");
      if (node->as.link.title.len) {
        cmark_strbuf_puts(html, " title=\"");
        houdini_escape_html0(html, node->as.link.title.data,
                             node->as.link.title.len, 0);
        cmark_strbuf_puts(html, "\"");
      }
      if (attrs.size) {
        cmark_strbuf_putc(html, ' ');
        cmark_strbuf_put(html, attrs.ptr, attrs.size);
      }
      cmark_strbuf_puts(html, ">");
    } else {
      cmark_strbuf_puts(html, "</a>");
    }
  }
  cmark_strbuf_free(&attrs);
}

/* Extract a LaTeX \includegraphics option string from the attribute body into
 * `out` (e.g. `[width=0.4\linewidth]`), if a `width=...` attribute is present.
 * A percent width N% becomes 0.NN\linewidth; any other value is emitted as-is. */
static void latex_image_opts(cmark_strbuf *out, const char *info) {
  const char *p = info, *v;
  size_t vlen;
  while (*p) {
    while (*p == ' ' || *p == '\t')
      p++;
    if (strncmp(p, "width=", 6) == 0) {
      p += 6;
      if (*p == '"') {
        p++;
        v = p;
        while (*p && *p != '"')
          p++;
        vlen = (size_t)(p - v);
      } else {
        v = p;
        while (*p && *p != ' ' && *p != '\t')
          p++;
        vlen = (size_t)(p - v);
      }
      cmark_strbuf_puts(out, "[width=");
      if (vlen > 0 && v[vlen - 1] == '%') {
        char buf[32];
        double pct = atof(v); /* leading digits, stops at % */
        snprintf(buf, sizeof(buf), "%g\\linewidth", pct / 100.0);
        cmark_strbuf_puts(out, buf);
      } else {
        cmark_strbuf_put(out, (const unsigned char *)v, (bufsize_t)vlen);
      }
      cmark_strbuf_puts(out, "]");
      return;
    }
    /* skip to next token */
    while (*p && *p != ' ' && *p != '\t') {
      if (*p == '"') {
        p++;
        while (*p && *p != '"')
          p++;
      }
      if (*p)
        p++;
    }
  }
}

static void latex_render(cmark_syntax_extension *extension,
                         cmark_renderer *renderer, cmark_node *node,
                         cmark_event_type ev_type, int options) {
  bool entering = (ev_type == CMARK_EVENT_ENTER);
  inlineattrs_data *data = (inlineattrs_data *)cmark_node_get_user_data(node);
  const char *url = cmark_node_get_url(node);
  (void)extension;
  (void)options;

  if (node->type == CMARK_NODE_IMAGE) {
    cmark_strbuf opts = CMARK_BUF_INIT(renderer->mem);
    if (!entering)
      return;
    latex_image_opts(&opts, data->info);
    renderer->out(renderer, node, "\\protect\\includegraphics", false, LITERAL);
    if (opts.size) {
      cmark_strbuf_putc(&opts, 0);
      renderer->out(renderer, node, (const char *)opts.ptr, false, LITERAL);
    }
    cmark_strbuf_free(&opts);
    renderer->out(renderer, node, "{", false, LITERAL);
    renderer->out(renderer, node, url, false, URL);
    renderer->out(renderer, node, "}", false, LITERAL);
    return;
  }

  /* LINK: empty href -> bare group; '#'-href -> \hyperlink; else \href.
   * Attributes are dropped for LaTeX (matching the previous R behaviour). */
  if (url[0] == 0) {
    renderer->out(renderer, node, entering ? "{" : "}", false, LITERAL);
  } else if (url[0] == '#') {
    if (entering) {
      renderer->out(renderer, node, "\\protect\\hyperlink{", false, LITERAL);
      renderer->out(renderer, node, url + 1, false, URL);
      renderer->out(renderer, node, "}{", false, LITERAL);
    } else {
      renderer->out(renderer, node, "}", false, LITERAL);
    }
  } else {
    if (entering) {
      renderer->out(renderer, node, "\\href{", false, LITERAL);
      renderer->out(renderer, node, url, false, URL);
      renderer->out(renderer, node, "}{", false, LITERAL);
    } else {
      renderer->out(renderer, node, "}", false, LITERAL);
    }
  }
}

cmark_syntax_extension *create_inlineattrs_extension(void) {
  cmark_syntax_extension *ext = cmark_syntax_extension_new("inlineattrs");

  cmark_syntax_extension_set_html_render_func(ext, html_render);
  cmark_syntax_extension_set_latex_render_func(ext, latex_render);
  cmark_syntax_extension_set_postprocess_func(ext, postprocess);

  return ext;
}
