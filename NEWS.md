# CHANGES IN litedown VERSION 0.3

- Added helper functions `pkg_desc()`, `pkg_news()`, `pkg_citation()`, and `pkg_manual()` to get various package information for building the full package documentation as a single-file book (thanks, @jangorecki @llrs #24, @TimTaylor #22).

- Section headings containing the class name "unlisted" will be excluded in the table of contents.

- Added back/forward/refresh/print buttons to the toolbar in the `litedown::roam()` preview interface.

- Added the JS asset [`@mathjax-config`](https://github.com/yihui/misc.js/blob/main/js/mathjax-config.js) to enable equation numbering by default when the JS math library is set to MathJax (thanks, @hturner, #32).

- Set `options(bitmapType = 'cairo')` in `fuse()` if `capabilities('cairo')` is TRUE, which will generate smaller bitmap plot files (e.g., `.png`) than using `quartz` or `Xlib`, and is also a safer option for `fuse()` to be executed in parallel (rstudio/rmarkdown#2561).

- Added a new vignette engine `litedown::book` to make it possible to build multiple vignettes into a book. To use this engine, declare `\VignetteEngine{litedown::book}` only in the book index file (e.g., `index.Rmd`) but not in other book chapter files.

- Added support for an array of multiple authors in the YAML metadata (thanks, @AlbertLei, #28). If the `author` field in YAML is an array of length > 1, each author will be written to a separate `<h2>` in HTML output, or concatenated by `\and` in LaTeX output. Note that you can also write multiple authors in a single string (e.g., `author: "Jane X and John Y"`) instead of using an array (`author: ["Jane X", "John Y"]`), in which case the string will be treated as a single author (they will be put inside a single `<h2>` in HTML output).

- Fixed the bug that the leading `-`, `+`, or `*` in a LaTeX math expression was recognized as the bullet list marker, which would invalidate the math expression (thanks, @hturner, #33).

- Changed the first `-` to `:` in automatically generated element IDs, including section, figure, and table IDs, e.g., the ID `sec-intro-methods` is changed to `sec:intro-methods`, and `fig-nice-plot` is changed to `fig:nice-plot`. You can still use `-` when manually assigning IDs to elements, e.g., `# Intro Methods {#sec-intro-methods}`. For backward compatibility, cross-references using `-` will be resolved if the `:` version of the ID can be found, e.g., `@sec-intro-methods` will be resolved to `@sec:intro-methods` if the former cannot be found but the latter can.

# CHANGES IN litedown VERSION 0.2

- A data frame (or matrix/tibble) wrapped in `I()` is fully printed to a table now by default. Without `I()`, data objects are truncated to 10 rows by default when printing to tables.

- When `options(litedown.fig.alt = TRUE)` and the chunk option `fig.alt` is unset, `fuse()` will emit reminders about the missing alt text for code chunks containing plots (thanks, @TimTaylor, #23). Providing alt text can improve the accessibility of images in HTML output. To avoid omitting the alt text inadvertently, you can set the option `litedown.fig.alt` in your `.Rprofile`.

- Added the meta variable `plain-title` for HTML output, which is the plain version of the document title (i.e., without HTML tags), and used in the `<title>` tag.

- Check boxes from `- [ ] ...` are no longer disabled in HTML output.

- The implicit latest version of jsdelivr resources will be resolved to an explicit version, e.g., `https://cdn.jsdelivr.net/npm/@xiee/utils/css/default.css` will be resolved to `https://cdn.jsdelivr.net/npm/@xiee/utils@X.Y.Z/css/default.css`, where `X.Y.Z` is the current latest version. This will make sure the HTML output containing jsdelivr resources is stable.

# CHANGES IN litedown VERSION 0.1

- Initial CRAN release.
