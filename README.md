```         
  ______  
 /   ⚡  \
/litedown\
\   ⚡    /
 \______/
```

# R Markdown Reimagined

<!-- badges: start -->

[![CRAN
release](https://www.r-pkg.org/badges/version/litedown)](https://cran.r-project.org/package=litedown)
[![litedown on
r-universe](https://yihui.r-universe.dev/badges/litedown)](https://yihui.r-universe.dev/litedown)
[![R-CMD-check](https://github.com/yihui/litedown/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/yihui/litedown/actions/workflows/R-CMD-check.yaml)
[![Build site](https://github.com/yihui/litedown/actions/workflows/litedown-site.yml/badge.svg)](https://github.com/yihui/litedown/actions/workflows/litedown-site.yml)
[![Codecov test
coverage](https://codecov.io/gh/yihui/litedown/branch/main/graph/badge.svg)](https://app.codecov.io/gh/yihui/litedown?branch=main)

<!-- badges: end -->

**litedown** turns Markdown and R Markdown into HTML and LaTeX/PDF, with no
**knitr** and no Pandoc. It imports only one R package, **xfun**, and
renders Markdown with a bundled and patched copy of the
[cmark-gfm](https://github.com/github/cmark-gfm) C library.

-   **Computing**: run code chunks and inline code in R, or in
    other languages via engines such as JavaScript, CSS, Mermaid, and shell
    commands. Chunk options control code, text output, figures, tables,
    caching, and even the order of execution.

-   **Markdown**: GFM plus math, footnotes, superscripts/subscripts, raw
    HTML/LaTeX blocks, element attributes, cross-references, citations, and
    callouts.

-   **Output**: single HTML documents (with resources embedded if you want),
    LaTeX and PDF, and lightweight HTML articles, slides, and paged HTML (for
    print), styled with small vanilla CSS/JS files from
    [lite.js](https://github.com/yihui/lite.js).

-   **Projects**: books and websites from a directory of documents with a
    `_litedown.yml`, including websites for R packages via `pkg_site()`.

-   **Workflow**: `roam()` previews and renders documents in a browser, with
    live reload, so you do not need a `Knit` button.

## Installation

``` r
install.packages('litedown')
```

The development version is available from r-universe:

``` r
install.packages('litedown', repos = 'https://yihui.r-universe.dev')
```

## A quick look

Save this as `hello.Rmd`:

````` markdown
---
title: Hello litedown
output: html
---

```{r}
x = 2
```

The area of a circle with radius `{r} x` is `{r} pi * x^2`.
`````

Then render it:

``` r
litedown::fuse('hello.Rmd')  # hello.Rmd -> hello.html
litedown::roam()             # or preview and render in a browser
```

## Main functions

| Function                                  | What it does                                  |
|:------------------------------------------|:----------------------------------------------|
| `fuse()`                                  | executes code in an R Markdown document, then renders it via `mark()` (like `rmarkdown::render()`) |
| `mark()`                                  | renders Markdown to HTML, LaTeX, etc. (like Pandoc) |
| `fiss()`                                  | extracts code from a document (like `knitr::purl()`) |
| `roam()`                                  | previews and renders documents in a browser   |
| `fuse_book()`, `fuse_site()`, `pkg_site()` | build books, websites, and package sites     |

If you do not compute in your documents, `mark()` alone is enough.

## Learn more

Visit the package site, <https://pkg.yihui.org/litedown/>, which has the
manual, the book *litedown: R Markdown Reimagined*, and examples.

## Scope

I try to keep the R package small, and most planned features have been
(re-)implemented so far, such as reports, slides, books, websites (including
package sites), and paged HTML documents, etc. See the set $R$ below:

$$\mathrm{litedown} = \min{\{R\}} + \{D_i\} - \{D_e\} + \{J\}$$

-   $R$ = [**knitr**](https://github.com/yihui/knitr) +
    [**evaluate**](https://github.com/r-lib/evaluate) +
    [**rmarkdown**](https://github.com/rstudio/rmarkdown) +
    [**bookdown**](https://github.com/rstudio/bookdown) +
    [**blogdown**](https://github.com/rstudio/blogdown) +
    [**pagedown**](https://github.com/rstudio/pagedown) +
    [**pkgdown**](https://github.com/r-lib/pkgdown) +
    [**xaringan**](https://github.com/yihui/xaringan) +
    [**tufte**](https://github.com/rstudio/tufte) +
    [**distill**](https://github.com/rstudio/distill) +
    [**htmlwidgets**](https://github.com/ramnathv/htmlwidgets)

-   $D_i$ = (internal dependencies) **xfun**

-   $D_e$ = (external dependencies) Pandoc + Bootstrap + jQuery + GitBook +
    Hugo + paged.js + remark.js + tufte.css + distill.js/.css + ...

-   $J$ = Lightweight [vanilla JavaScript/CSS](https://github.com/yihui/lite.js)

Much of the functionality lives in CSS/JS, which makes **litedown** highly
extensible and customizable: see [Chapter 5 of the
book](https://pkg.yihui.org/litedown/book/#chp:assets) for how to use the
default assets and write your own. You are welcome to write your own CSS/JS, or
contribute yours to [**lite.js**](https://github.com/yihui/lite.js) so that
everyone can use it.

Feature requests are welcome, too. I will take votes into consideration, so
please upvote the features you like.

## Acknowledgments

**litedown** was directly inspired by the packages in the set $R$ above. I wrote
or co-wrote most of them, and learned a lot from the rest. Thanks to all their
authors and contributors, and to the users whose questions, bug reports, and
feedback over the years shaped the design of this package.

## License

The **litedown** package is licensed under MIT.
