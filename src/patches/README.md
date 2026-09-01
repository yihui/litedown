# Vendored cmark-gfm

The C library under `src/cmark/` and `src/extensions/` is a copy of
[cmark-gfm](https://github.com/github/cmark-gfm), the GitHub fork of John
MacFarlane's [cmark](https://github.com/commonmark/cmark) reference
implementation of CommonMark.

The sources were taken from the [commonmark R
package](https://github.com/r-lib/commonmark) (which vendors cmark-gfm the same
way), so that litedown can call the C library directly instead of depending on
the commonmark package. This lets us add extensions and patch bugs at the C
level rather than working around them in R.

- Upstream cmark-gfm version: **0.29.0.gfm.6**
  (see `src/cmark/cmark-gfm_version.h`).
- License: BSD-2-clause, see `src/cmark/COPYING`.

## Files not from upstream cmark-gfm

These are litedown's own glue / bindings (do not overwrite on sync):

- `src/init.c`      — native routine registration (`R_init_litedown`).
- `src/wrapper.c`   — `R_render_markdown()`, renders to a string. Copied from
  the commonmark package.
- `src/extensions.c`— `R_list_extensions()`. Copied from the commonmark package.
- `src/parse.c`     — `R_parse_markdown()`, returns the parse tree (AST) to R as
  a nested list. Written for litedown.
- `src/Makevars`    — build rules (copied from the commonmark package).

## Local patches

Local modifications to the vendored C are kept as diffs under `src/patches/`
and are **already applied** to the committed sources (mirroring the commonmark
package, which also commits pre-patched sources). The diffs are retained so the
changes can be re-applied after syncing with a newer upstream, and so they can
be submitted upstream.

Current patches (see `src/patches/apply.sh`):

- `362.diff`             — GFM PR #362: expose footnote ids in XML/HTML
  (from upstream cmark-gfm, not yet merged).
- `latex-footnotes.diff` — commonmark PR #32: LaTeX footnotes via
  `\footnotemark` / `\footnotetext`.
- `inline-sourcepos-cols.diff` — litedown: fix inline source positions
  (columns / end lines) on indented continuation lines. To be submitted
  upstream to github/cmark-gfm.

Prefer adding new features as standalone files under `src/extensions/` rather
than as patches to core files: extensions survive upstream syncs untouched,
while patches are merge-conflict debt. Reserve patches for changes that must
touch core parser files, and file them upstream first when possible.

## How to sync with a newer upstream

1. Obtain the new cmark-gfm sources (e.g. from a new commonmark package release
   or from the cmark-gfm repo).
2. Replace the contents of `src/cmark/` and `src/extensions/` (keep `COPYING`
   and litedown's own extension files).
3. Re-apply the patches: `cd src/patches && ./apply.sh`
   (adjust hunks that no longer apply; drop any patch merged upstream).
4. Re-check the glue files above against the commonmark package for API changes.
5. Rebuild and run the test suite.
