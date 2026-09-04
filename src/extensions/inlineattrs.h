#ifndef LITEDOWN_INLINEATTRS_H
#define LITEDOWN_INLINEATTRS_H

#include "cmark-gfm-core-extensions.h"

/* Pandoc-style inline attributes ({#id .class key=val}) on links and images.
 * See inlineattrs.c for details. */
cmark_syntax_extension *create_inlineattrs_extension(void);

#endif
