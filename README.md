```         
  ______  
 /   ⚡  \
/litedown\
\   ⚡    /
 \______/
```

# R Markdown Reimagined

<!-- badges: start -->

[![R-CMD-check](https://github.com/yihui/litedown/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/yihui/litedown/actions/workflows/R-CMD-check.yaml)
[![CRAN
release](https://www.r-pkg.org/badges/version/litedown)](https://cran.r-project.org/package=litedown)
[![litedown on
r-universe](https://yihui.r-universe.dev/badges/litedown)](https://yihui.r-universe.dev/litedown)

<!-- badges: end -->

This package provides a trimmed-down and reimagined implementation of [R
Markdown](https://rmarkdown.rstudio.com). It is much more lightweight, at the
price of dropping some features. It does not depend on the R package **knitr**
or the system package Pandoc.

Please consider this package experimental for now. The documentation is also
very incomplete and still under development.

## Usage

### Installation

At the moment, you are recommended to install the development version from
r-universe:

``` r
install.packages('litedown', repos = c('https://yihui.r-universe.dev', 'https://cloud.r-project.org'))
```

### Markdown rendering

The function `litedown::mark()` is based on the R package
[**commonmark**](https://github.com/r-lib/commonmark), and renders Markdown to
various output formats supported by **commonmark**, which are primarily HTML and
LaTeX. MS Office formats are not supported.

### Knitting

R Markdown documents need to be knitted to Markdown before being rendered to a
target output format. The function `litedown::fuse()` plays a role similar to
`knitr::knit()` and `rmarkdown::render()`. It fuses program code with
narratives, i.e., it executes code in the source document and interweaves
results with narratives in the output document.

### Previewing

Try `litedown::roam()`.

## Scope

We want to limit the scope of this package. Most planned features have been
implemented so far. Please feel free to file feature requests anyway, but we may
be a little conservative when considering them (we will take votes into
consideration, so please upvote features you like).

## License

The **litedown** package is licensed under MIT.
