#ifndef LITEDOWN_EXTENSIONS_H
#define LITEDOWN_EXTENSIONS_H

#include "cmark-gfm.h"

/* Register litedown's own syntax extensions with the cmark registry. Safe to
 * call more than once (registration happens only on the first call). */
void litedown_extensions_ensure_registered(void);

/* Attach the named extension to `parser`, with litedown-specific remapping for
 * the tilde-owning features: the names "subscript" and "strikethrough" both map
 * to litedown's single '~'-owning "subscript" extension (attached at most once)
 * and set the corresponding LITEDOWN_OPT_* bit on the parser so the two
 * features can be toggled independently; the upstream "strikethrough" extension
 * is never attached (it would fight over '~'). Returns 1 on success, 0 if the
 * extension name is unknown (caller should error). */
int litedown_attach_extension(cmark_parser *parser, const char *name);

#endif
