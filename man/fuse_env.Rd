% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/package.R
\name{fuse_env}
\alias{fuse_env}
\title{The \code{fuse()} environment}
\usage{
fuse_env()
}
\value{
When called during \code{fuse()}, it returns the \code{envir} argument value of
\code{fuse()}. When called outside \code{fuse()}, it returns the global environment.
}
\description{
Get the environment passed to the \code{envir} argument of \code{\link[=fuse]{fuse()}}, i.e., the
environment in which code chunks and inline code are evaluated.
}
