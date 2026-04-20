# Repository Instructions for Copilot

## Build and Test Instructions

``` bash
# Build the R package
R CMD build .

# Install the package
R CMD INSTALL *_*.tar.gz

# Test the package
cd tests
Rscript *.R
```

Tests are typically in `tests/testit/test-*.R` (for each `R/foo.R`, there is a
corresponding `tests/testit/test-foo.R`). In certain cases they may be in other
directories, e.g., `tests/test-cran/` (for tests to run on anywhere, including
CRAN) and `tests/test-ci/` (tests to run on CI only because they might fail on
CRAN due to Internet connection or resource limits). The conditioning is done in
top-level `*.R` under `tests/`, e.g.,

``` r
# tests/test-cran.R
testit::test_pkg(dir = 'test-cran')

# tests/test-ci.R
if (tolower(Sys.getenv('CI')) == 'true') testit::test_pkg(dir = 'test-ci')
```

Tests consist of assertions of this form:

``` r
library(testit)

assert('expectation message', {
  actual = FUN(args, ...)
  (actual %==% expected)
  # more tests of the above form, e.g.,
  (length(res) %==% 3L)
})
```

-   Use `has_error()` instead of `tryCatch()` for error testing
-   Never use `:::` to access internal functions in tests; testit exposes
    internal functions automatically, so call them directly

## Important Conventions

### R Code Style

1.  **Assignment**: Use `=` instead of `<-` for assignment
2.  **Strings**: Use single quotes for strings (e.g., `'text'`)
3.  **Indentation**: Use 2 spaces (not 4 spaces or tabs)
4.  **Compact code**: Avoid `{}` for single-expression if statements; prefer
    compact forms when possible
5.  **Roxygen documentation**: Don't use `@description` or `@details` explicitly
    — just write the description text directly after the title. Don't use
    `@title` either.
6.  **Examples**: Avoid `\dontrun{}` unless absolutely necessary. Prefer
    runnable examples that can be tested automatically.
7.  **Function definitions**: For functions with many arguments, break the line
    right after the opening `(`, indent arguments by 2 spaces, and try to wrap
    them at 80-char width.
8.  **Re-wrap code**: Always re-wrap the code after making changes to maintain
    consistent formatting and line length.
9.  **Implicit NULL**: Don't write `if (cond) foo else NULL`; the `else NULL` is
    unnecessary since R's `if` without `else` already returns `NULL`. Never
    write `return(NULL)`; use `return()` instead since R functions return `NULL`
    by default when no value is given.
10. **US spelling**: Use US spelling throughout all documentation, code
    comments, and example text (e.g., "color" not "colour", "center" not
    "centre", "summarize" not "summarise").
11. **DRY (Don't Repeat Yourself)**: Never duplicate code. When the same logic
    appears more than once, factor it into a shared helper function. This
    applies to expressions, patterns, and multi-line blocks alike.

### Check list

Always send a pull request, unless you are told otherwise. For each PR:

1.  **Every change must have tests**: Every code change must come with
    corresponding tests. If you add or fix a function, add assertions in the
    test file that cover the new or fixed behavior. Tests are the first place to
    catch regressions and errors.
2.  **Always re-roxygenize**: Run `roxygen2::roxygenize()` after changing any
    roxygen documentation to update man files
3.  **MANDATORY: `R CMD check` before `git push`**: You MUST run a comprehensive
    `R CMD check` successfully before submitting ANY code changes.
4.  **MANDATORY: Wait for CI to be green**: After pushing code, you MUST wait
    for GitHub Actions CI to complete successfully before claiming the task is
    done. Do not wait more than 5 minutes for any single CI job; if it hasn't
    finished, skip it and continue your work. Fix problems in CI as soon as any
    job has failed instead of waiting for all jobs to finish.
5.  **MANDATORY: Merge latest main before pushing**: Before pushing to a branch
    or PR, always pull and merge the latest main branch. If there are merge
    conflicts, resolve them before pushing.
6.  **Bump version in PRs**: Bump the patch version number in DESCRIPTION once
    per PR (on the first commit or when you first make changes), not on every
    commit to the PR
7.  **Never commit irrelevant files**: Don't run `git add .` blindly, as that
    might add irrelevant files such as generated output or other artifacts. Only
    add the ones you modified or created explicitly. Normally changes generated
    automatically by roxygen2 are the only exception (they should be committed).
8.  **Update NEWS.md**: When making changes, make sure to update `NEWS.md`
    accordingly to document what changed. The first heading in NEWS.md always
    represents the dev version and must be of the form `# PKG x.y` where PKG is
    the package name and x.y is the next version to be released to CRAN (note:
    x.y, not x.y.0). Usually y is bumped from the current minor version, e.g.,
    if the current dev version is 1.8.3, the next CRAN release is expected to be
    1.9.
