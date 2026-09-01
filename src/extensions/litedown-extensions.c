/* Registration of litedown's own cmark extensions.
 *
 * Kept separate from upstream's core-extensions.c (which src/patches/sync.sh
 * overwrites) so that litedown's extensions survive an upstream sync. init.c
 * calls litedown_extensions_ensure_registered() alongside the core one.
 */

#include "litedown-extensions.h"
#include "math.h"
#include "registry.h"
#include "plugin.h"

static int litedown_extensions_registration(cmark_plugin *plugin) {
  cmark_plugin_register_syntax_extension(plugin, create_math_extension());
  return 1;
}

void litedown_extensions_ensure_registered(void) {
  static int registered = 0;

  if (!registered) {
    cmark_register_plugin(litedown_extensions_registration);
    registered = 1;
  }
}
