% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fuse.R
\name{raw_text}
\alias{raw_text}
\title{Mark a character vector as raw output}
\usage{
raw_text(x, format = NULL)
}
\arguments{
\item{x}{A character vector (each element will be treated as a line).}

\item{format}{An output format name, e.g., \code{html} or \code{latex}. If provided,
\code{x} will be wrapped in a fenced code block, e.g., \verb{ ```\{=html\}}.}
}
\value{
A character vector with a special class to indicate that it should be
treated as raw output.
}
\description{
This function should be called inside a code chunk, and its effect is the
same as the chunk option \code{results = "asis"}. The input character vector will
be written verbatim to the output (and interpreted as Markdown).
}
\examples{
litedown::raw_text(c("**This**", "_is_", "[Markdown](#)."))
litedown::raw_text("<b>Bold</b>", "html")
litedown::raw_text("\\\\textbf{Bold}", "latex")
}
