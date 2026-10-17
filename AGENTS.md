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
4. **Run tests**: `Rscript tests/test-all.R`
5. **Clean up artifacts**: `rm -rf *.tar.gz *.Rcheck/`
