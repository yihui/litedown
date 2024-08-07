% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/format.R
\name{html_format}
\alias{html_format}
\alias{latex_format}
\title{Output formats in YAML metadata}
\usage{
html_format(options = NULL, meta = NULL, template = NULL, keep_md = FALSE)

latex_format(
  options = NULL,
  meta = NULL,
  template = NULL,
  keep_md = FALSE,
  keep_tex = FALSE,
  latex_engine = "xelatex",
  citation_package = "natbib"
)
}
\arguments{
\item{meta, options}{Arguments to be passed to \code{\link[=mark]{mark()}}.}

\item{template}{A template file path.}

\item{keep_md, keep_tex}{Whether to keep the intermediate \file{.md} and
\file{.tex} files generated from \file{.Rmd}.}

\item{latex_engine}{The LaTeX engine to compile \file{.tex} to \file{.pdf}.}

\item{citation_package}{The LaTeX package for processing citations. Possible
values are \code{none}, \code{natbib}, and \code{biblatex}.}
}
\value{
An R Markdown output format.
}
\description{
The primary output formats of \pkg{litedown} are HTML and LaTeX. These output
formats can be configured in the \code{output} field of the YAML metadata of the
Markdown document.
}
\details{
The output format functions have two purposes. The main purpose is to make it
possible (and easier) to configure the output formats using YAML metadata
inside a document, e.g.,

\if{html}{\out{<div class="sourceCode yaml">}}\preformatted{---
output:
  litedown::html_format:
    options:
      toc: true
    keep_md: true
  litedown::latex_format:
    latex_engine: pdflatex
---
}\if{html}{\out{</div>}}

The secondary purpose is for \pkg{rmarkdown} users to render R Markdown via
\code{\link[knitr:knit]{knitr::knit()}} and \code{\link[=mark]{mark()}} (instead of Pandoc), and also use the \code{Knit}
button in RStudio. Although you can render R Markdown to Markdown via either
\code{knitr::knit()} or \code{\link[=fuse]{fuse()}}, please note that the two ways are not 100\%
compatible with each other. If you choose to use \pkg{litedown}, we recommend
that you use \code{fuse()} instead. If you want \code{fuse()} to work with the \code{Knit}
button in RStudio, you have to add a special field to YAML:

\if{html}{\out{<div class="sourceCode yaml">}}\preformatted{---
knit: litedown:::knit
---
}\if{html}{\out{</div>}}

Without this field, RStudio will use \pkg{knitr} to render R Markdown.
}
