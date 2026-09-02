#ifndef LITEDOWN_MATH_H
#define LITEDOWN_MATH_H

#include "cmark-gfm-core-extensions.h"

extern cmark_node_type CMARK_NODE_MATH_INLINE;
extern cmark_node_type CMARK_NODE_MATH_DISPLAY;
extern cmark_node_type CMARK_NODE_MATH_ENV;
cmark_syntax_extension *create_math_extension(void);

/* Whether the NUL-terminated string `s` is composed entirely of top-level LaTeX
 * math environments (\begin{...} ... \end{...}) with no stray text at depth 0.
 * Shared with the raw-blocks extension, which applies the same rule to decide
 * whether a raw {=latex}/{=tex} block should still render as math in HTML. */
int litedown_is_math_env(const char *s);

#endif
