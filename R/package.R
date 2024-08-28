#' A lightweight version of R Markdown
#'
#' Markdown is a plain-text format that can be converted to HTML and other
#' formats. This package can render R Markdown to Markdown, and then to an
#' output document format. The main differences between this package and
#' \pkg{rmarkdown} are that it does not use Pandoc or \pkg{knitr} (i.e., fewer
#' dependencies), and it also has fewer Markdown features.
#' @importFrom xfun base64_uri csv_options download_cache fenced_block
#'   fenced_div file_exists file_ext grep_sub in_dir loadable new_record
#'   normalize_path prose_index raw_string read_all read_utf8 record_print
#'   Rscript_call sans_ext split_lines with_ext write_utf8
'_PACKAGE'

# an internal environment to store some intermediate objects
.env = new_env()

#' The `fuse()` environment
#'
#' Get the environment passed to the `envir` argument of [fuse()], i.e., the
#' environment in which code chunks and inline code are evaluated.
#' @return When called during `fuse()`, it returns the `envir` argument value of
#'   `fuse()`. When called outside `fuse()`, it returns the global environment.
#' @export
fuse_env = function() .env$global %||% globalenv()

#' @export
record_print.data.frame = function(x, ...) {
  asis = inherits(x, 'AsIs')
  if (is.null(getOption('xfun.md_table.limit'))) {
    opts = options(xfun.md_table.limit = if (!asis) 10)
    on.exit(options(opts), add = TRUE)
  }
  if (asis) class(x) = setdiff(class(x), 'AsIs')
  if (inherits(x, 'tbl_df')) x = as.data.frame(x)
  tab = xfun::md_table(x, ...)
  opt = reactor()
  tab = add_cap(tab, opt$tab.cap, opt$label, opt$cap.pos %||% 'top', opt$tab.env, 'tab')
  new_record(c(tab, ''), 'asis')
}

#' @export
record_print.matrix = record_print.data.frame

#' @export
record_print.tbl_df = record_print.data.frame

#' @export
record_print.knitr_kable = function(x, ...) {
  if ((fmt <- attr(x, 'format')) %in% c('html', 'latex'))
    x = fenced_block(x, paste0('=', fmt))
  new_record(c(x, ''), 'asis')
}

# register vignette engines
.onLoad = function(lib, pkg) {
  tools::vignetteEngine(
    'vignette', vig_fun(TRUE), vig_fun(FALSE), '[.]R?md$',
    aspell = list(filter = vig_filter)
  )
}

# weave or tangle?
vig_fun = function(weave = TRUE) {
  function(file, quiet = FALSE, ...) {
    empty_file = function() write_utf8(character(), with_ext(file, '.R'))
    # fuse() .Rmd and mark() .md
    if (grepl('[.]Rmd$', file)) {
      if (weave) fuse(file, quiet = quiet, envir = globalenv()) else {
        if (getRversion() <= '3.2.5') empty_file() else fiss(file)
      }
    } else {
      if (weave) mark(file) else empty_file()
    }
  }
}

# filter out code from document so aspell() won't spell check code
vig_filter = function(ifile, encoding) {
  res = crack(ifile)
  res = lapply(res, function(x) {
    if (x$type == 'code_chunk') return(rep('', length(x$source)))
    if (is.character(x$source)) x$source else {
      one_string(uapply(x$source, function(s) {
        if (is.character(s)) s else ''
      }), '')
    }
  })
  structure(split_lines(unlist(res)), control = '-H -t')
}
