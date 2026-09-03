#!/bin/sh

# Apply litedown's patches to the vendored cmark-gfm sources. Run from this
# directory (src/patches). Patches use paths of the form a/cmark/FILE and
# a/extensions/FILE and are applied with -p1 relative to ../ (i.e. src/).
#
# sync.sh calls this after copying fresh upstream sources. The patches are also
# already applied to the committed sources, so a plain checkout builds without
# running this.

set -e

apply() { patch -p1 --no-backup-if-mismatch -d .. < "$1"; }

# Expose footnote ids in XML/HTML. Upstream PR:
# https://github.com/github/cmark-gfm/pull/362
apply 362.diff
# Support footnotes for LaTeX. From commonmark PR:
# https://github.com/r-lib/commonmark/pull/32
apply latex-footnotes.diff
# Fix inline source positions on indented continuation lines (litedown).
# To be submitted upstream to github/cmark-gfm.
apply inline-sourcepos-cols.diff
# End a type 6/7 HTML block at an opening code fence (litedown), so that
# </p>\n``` is parsed as HTML followed by a code block. To be submitted
# upstream to github/cmark-gfm.
apply html-block-code-fence-end.diff
# Render task list checkboxes without disabled="" (litedown), so they are
# interactive. litedown-specific (GitHub renders them disabled), not for upstream.
apply tasklist-no-disable.diff
