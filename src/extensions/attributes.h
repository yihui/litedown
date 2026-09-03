#ifndef LITEDOWN_ATTRIBUTES_H
#define LITEDOWN_ATTRIBUTES_H

#include "cmark-gfm-core-extensions.h"
#include "buffer.h"

cmark_syntax_extension *create_attributes_extension(void);

/* Append an HTML attribute string (class, id, then key=value tokens) built from
 * a Pandoc-style attribute list body to `out`. Shared by the code-block render
 * func and R's convert_attrs() via the R_convert_attrs binding. See the
 * definition in attributes.c for the exact ordering/escaping rules. */
void litedown_render_attrs(cmark_strbuf *out, const unsigned char *d,
                           bufsize_t len, const char *class_prefix);

#endif
