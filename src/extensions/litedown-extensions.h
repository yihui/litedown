#ifndef LITEDOWN_EXTENSIONS_H
#define LITEDOWN_EXTENSIONS_H

/* Register litedown's own syntax extensions with the cmark registry. Safe to
 * call more than once (registration happens only on the first call). */
void litedown_extensions_ensure_registered(void);

#endif
