# A lightweight version of R Markdown

<!-- badges: start -->

[![R-CMD-check](https://github.com/yihui/litedown/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/yihui/litedown/actions/workflows/R-CMD-check.yaml)
[![CRAN
release](https://www.r-pkg.org/badges/version/litedown)](https://cran.r-project.org/package=litedown)

<!-- badges: end -->

```         
  ______  
 /   ⚡  \
/litedown\
\   ⚡    /
 \______/
```

## Overview

This package provides a trimmed-down implementation of [R
Markdown](https://rmarkdown.rstudio.com). It is much more lightweight and has
fewer features. It does not depend on the R package **knitr** or the system
package Pandoc.

``` r
install.packages('litedown', repos = 'https://yihui.r-universe.dev')
```

### Markdown rendering

The function `litedown::mark()` is based on the R package
[**commonmark**](https://github.com/r-lib/commonmark), and renders Markdown to
various output formats supported by **commonmark**; `mark_html()` is a wrapper
function to render Markdown to HTML, and `mark_latex()` is a wrapper function
for LaTeX output. HTML and LaTeX are the primary output formats supported by
this package. MS Office formats are not supported.

Some useful Markdown features missing in **commonmark** have been added to this
package, including LaTeX math, raw HTML/LaTeX blocks, fenced `Div`s, and
heading/image attributes, etc. The Markdown rendering behavior can be controlled
by several options, e.g., whether to enable the table of contents. It also
supports custom HTML/LaTeX templates. To get a full introduction, please read
[the `intro`
vignette](https://cran.r-project.org/package=litedown/vignettes/intro.html).

You may also read [the `article`
vignette](https://cran.r-project.org/package=litedown/vignettes/article.html)
and [the `slides`
vignette](https://cran.r-project.org/package=litedown/vignettes/slides.html) to
learn more about possible HTML applications based on this package.

### Knitting

R Markdown documents need to be knitted to Markdown before being rendered to a
target output format. The function `litedown::fuse()` plays a role similar to
`knitr::knit()`. It fuses program code with narratives, i.e., it executes all
code in the source document and interweaves results with narratives in the
output document.

## Scope

It is a deliberate design choice to keep this package lightweight, to make it
relatively easy to use and simple to maintain. The functions `mark()` and
`fuse()` can be viewed as significantly trimmed-down versions of Pandoc and
**knitr**, respectively.

Please feel free to file feature requests anyway, but we may be a little
conservative when considering them (we will take votes into consideration, so
please upvote features you like). It is definitely not the goal to become a
substitute of tools based on **knitr** and Pandoc, such as **rmarkdown** and
[Quarto](https://quarto.org). To some degree, this package is intended for
minimalists, and does not aim at rich features. If you are not sure if you
should choose **litedown** or **rmarkdown**/Quarto, you may want to choose the
latter (especially Quarto).

## License

The **litedown** package is licensed under MIT.
