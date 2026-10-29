# Repository Instructions

## Test Instructions

```bash
Rscript tests/test-all.R
```

Tests live in `tests/test-cran/test-*.R` (run on CRAN) and `tests/test-ci/test-*.R`
(CI only). The top-level runner is `tests/test-all.R`:

```r
library(testit)
test_pkg('litedown', 'test-cran')
if (tolower(Sys.getenv('CI')) == 'true') test_pkg('litedown', 'test-ci')
```

Test assertions use `testit`:

```r
assert('message', {
  (actual %==% expected)
})
```

- Only parenthesized expressions `(...)` in `assert()` are checked; other
  statements (setup, `on.exit()`) run as usual, so no `local()` wrapper is needed.
- To test whether output matches a string or pattern, prefer `has_match(x,
  pattern)` over `grepl()`: on mismatch it prints `x`, so we can see what went
  wrong, whereas `grepl()` only gives a vague `TRUE`/`FALSE`.
- Use `has_error()` instead of `tryCatch()` for error testing.
- Never use `:::` to access internal functions; `testit` exposes them automatically.

## R Code Style

1. **Assignment**: `=` not `<-`
2. **Strings**: single quotes in source code (e.g. `'text'`)
3. **Indentation**: 2 spaces
4. **Compact code**: no `{}` for single-expression `if`; no `else NULL`; no `return(NULL)`
5. **Roxygen**: no `@description`, `@details`, or `@title` tags; write description directly
6. **Examples**: double quotes in `@examples` code; avoid `\dontrun{}`
7. **Line length**: wrap at 80 chars (2-space continuation indent)
8. **US spelling**: "color" not "colour", etc.
9. **DRY**: factor shared logic into helpers; never duplicate code blocks

## Git workflow

1. Never force push unless explicitly told to.
2. Never create a new branch or PR without confirming with the user first.
3. Activate the pre-commit hook once per clone: `git config core.hooksPath .githooks`

## PR checklist

1. **Tests**: every code change needs corresponding test assertions.
2. **Re-roxygenize**: run `roxygen2::roxygenize('.')` and `./.githooks/pre-commit` after changing roxygen docs.
3. **Update NEWS.md**: first heading is always `# litedown x.y` (dev version).
   Keep each entry short and user-facing: say what the feature is for and how
   to use it, not how it works (no implementation details or edge cases; those
   belong in the docs). Refer to docs and examples by links on text (e.g.,
   `[details](https://pkg.yihui.org/litedown/book/#sec:foo)`) to their HTML
   pages, not bare URLs or source file names.
4. **Run tests**: `Rscript tests/test-all.R`
5. **Clean up artifacts**: `rm -rf *.tar.gz *.Rcheck/`
6. **Site groups**: the manual and examples pages of the site are grouped in
   `site/_litedown.yml` (fields `manual` and `examples` under `site`). When you
   add a new exported function (help page), add its topic to a group under
   `manual`; when you add a new file under `examples/`, add it to a topic group
   under `examples` by its number prefix (e.g., `"029-*"`), or create a new
   group if no group fits. Items that do not belong to any group are put in an
   `Other` group, and a name in the config that matches nothing is an error, so
   also update the config when you rename or delete a function or an example.
