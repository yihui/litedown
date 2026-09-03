/* Registration of litedown's own cmark extensions.
 *
 * Kept separate from upstream's core-extensions.c (which src/patches/sync.sh
 * overwrites) so that litedown's extensions survive an upstream sync. init.c
 * calls litedown_extensions_ensure_registered() alongside the core one.
 */

#include "litedown-extensions.h"
#include "math.h"
#include "subsup.h"
#include "rawblock.h"
#include "attributes.h"
#include "latexfootnotes.h"
#include "registry.h"
#include "plugin.h"
#include "parser.h"
#include "syntax_extension.h"
#include <string.h>

static int litedown_extensions_registration(cmark_plugin *plugin) {
  cmark_plugin_register_syntax_extension(plugin, create_math_extension());
  cmark_plugin_register_syntax_extension(plugin, create_superscript_extension());
  cmark_plugin_register_syntax_extension(plugin, create_subscript_extension());
  cmark_plugin_register_syntax_extension(plugin, create_rawblock_extension());
  cmark_plugin_register_syntax_extension(plugin, create_attributes_extension());
  cmark_plugin_register_syntax_extension(plugin, create_latexfootnotes_extension());
  return 1;
}

void litedown_extensions_ensure_registered(void) {
  static int registered = 0;

  if (!registered) {
    cmark_register_plugin(litedown_extensions_registration);
    registered = 1;
  }
}

/* Return the "subscript" extension only if it is not already attached to the
 * parser, so requesting both "subscript" and "strikethrough" attaches it once. */
static cmark_syntax_extension *subsup_once(cmark_parser *parser) {
  cmark_llist *tmp;
  for (tmp = parser->syntax_extensions; tmp; tmp = tmp->next) {
    cmark_syntax_extension *e = (cmark_syntax_extension *)tmp->data;
    if (e->name && strcmp(e->name, "subscript") == 0)
      return NULL;
  }
  return cmark_find_syntax_extension("subscript");
}

int litedown_attach_extension(cmark_parser *parser, const char *name) {
  /* "subscript" and "strikethrough" both route to the one '~'-owning
   * extension; set the feature bit and attach that extension at most once. */
  if (strcmp(name, "subscript") == 0 || strcmp(name, "strikethrough") == 0) {
    cmark_syntax_extension *ext = subsup_once(parser);
    parser->options |= (strcmp(name, "subscript") == 0)
                           ? LITEDOWN_OPT_SUBSCRIPT
                           : LITEDOWN_OPT_STRIKETHROUGH;
    if (ext)
      cmark_parser_attach_syntax_extension(parser, ext);
    return 1;
  }
  {
    cmark_syntax_extension *ext = cmark_find_syntax_extension(name);
    if (!ext)
      return 0;
    cmark_parser_attach_syntax_extension(parser, ext);
    return 1;
  }
}
