#!/bin/sh

# Upstream PR: https://github.com/github/cmark-gfm/pull/362
patch -p2 -d ../cmark < 362.diff
# Support footnotes for LaTeX: https://github.com/r-lib/commonmark/pull/32
patch -p2 -d ../cmark < latex-footnotes.diff
# Fix inline source positions on indented continuation lines (litedown)
patch -p1 -d ../cmark < inline-sourcepos-cols.diff
