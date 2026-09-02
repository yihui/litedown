#ifndef LITEDOWN_SUBSUP_H
#define LITEDOWN_SUBSUP_H

#include "cmark-gfm-core-extensions.h"

/* Private parser option bits (litedown-only; picked from the free low bits not
 * used by any CMARK_OPT_*). The 'subscript' extension owns '~' and provides
 * both single-tilde subscript and double-tilde strikethrough; these bits let
 * the two be toggled independently even though one extension handles both.
 * They are set by litedown's wrapper.c / parse.c from the requested extension
 * list and read by match_sub(). */
#define LITEDOWN_OPT_SUBSCRIPT     (1 << 6)
#define LITEDOWN_OPT_STRIKETHROUGH (1 << 7)

extern cmark_node_type CMARK_NODE_SUPERSCRIPT;
extern cmark_node_type CMARK_NODE_SUBSCRIPT;
extern cmark_node_type CMARK_NODE_STRIKETHROUGH2;

cmark_syntax_extension *create_superscript_extension(void);
cmark_syntax_extension *create_subscript_extension(void);

#endif
