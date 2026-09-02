#ifndef LITEDOWN_MATH_H
#define LITEDOWN_MATH_H

#include "cmark-gfm-core-extensions.h"

extern cmark_node_type CMARK_NODE_MATH_INLINE;
extern cmark_node_type CMARK_NODE_MATH_DISPLAY;
extern cmark_node_type CMARK_NODE_MATH_ENV;
cmark_syntax_extension *create_math_extension(void);

#endif
