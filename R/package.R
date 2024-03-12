#' A lightweight version of R Markdown
#'
#' \pkg{Markdown} is a plain-text formatting syntax that can be converted to
#' HTML and other formats. This package can render R Markdown to Markdown, and
#' then to an output document format. The main differences between this package
#' and \pkg{rmarkdown} are that it does not use Pandoc or \pkg{knitr} (i.e.,
#' fewer dependencies), and it also has fewer Markdown features.
'_PACKAGE'

.env = new.env(parent = emptyenv())
