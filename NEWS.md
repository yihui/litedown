# CHANGES IN litedown VERSION 0.3

- Added back/forward/refresh/print buttons to the toolbar in the `litedown::roam()` preview interface.

# CHANGES IN litedown VERSION 0.2

- A data frame (or matrix/tibble) wrapped in `I()` is fully printed to a table now by default. Without `I()`, data objects are truncated to 10 rows by default when printing to tables.

- When `options(litedown.fig.alt = TRUE)` and the chunk option `fig.alt` is unset, `fuse()` will emit reminders about the missing alt text for code chunks containing plots (thanks, @TimTaylor, #23). Providing alt text can improve the accessibility of images in HTML output. To avoid omitting the alt text inadvertently, you can set the option `litedown.fig.alt` in your `.Rprofile`.

- Added the meta variable `plain-title` for HTML output, which is the plain version of the document title (i.e., without HTML tags), and used in the `<title>` tag.

- Check boxes from `- [ ] ...` are no longer disabled in HTML output.

- The implicit latest version of jsdelivr resources will be resolved to an explicit version, e.g., `https://cdn.jsdelivr.net/npm/@xiee/utils/css/default.css` will be resolved to `https://cdn.jsdelivr.net/npm/@xiee/utils@X.Y.Z/css/default.css`, where `X.Y.Z` is the current latest version. This will make sure the HTML output containing jsdelivr resources is stable.

# CHANGES IN litedown VERSION 0.1

- Initial CRAN release.
