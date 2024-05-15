#' A lightweight version of R Markdown
#'
#' \pkg{Markdown} is a plain-text formatting syntax that can be converted to
#' HTML and other formats. This package can render R Markdown to Markdown, and
#' then to an output document format. The main differences between this package
#' and \pkg{rmarkdown} are that it does not use Pandoc or \pkg{knitr} (i.e.,
#' fewer dependencies), and it also has fewer Markdown features.
#' @importFrom xfun base64_uri csv_options download_cache fenced_block
#'   fenced_div file_exists file_ext grep_sub in_dir loadable new_record
#'   normalize_path prose_index raw_string read_all read_utf8 record_print
#'   sans_ext split_lines with_ext write_utf8
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
  if (is.null(getOption('xfun.md_table.limit'))) {
    opts = options(xfun.md_table.limit = 10); on.exit(options(opts), add = TRUE)
  }
  tab = xfun::md_table(x, ...)
  opt = reactor()
  if (length(cap <- opt$tab.cap)) {
    cap = fenced_div(add_ref(opt$label, 'tab', cap), '.tab-caption')
    tab = if (opt$tab.pos == 'top') c(cap, '', tab) else c(tab, '', cap)
    tab = fenced_div(tab, c(opt$tab.env %||% '.table', sprintf('#tab-%s', opt$label)))
  }
  new_record(c(tab, ''), 'asis')
}

#' @export
record_print.matrix = record_print.data.frame

#' @export
record_print.tbl_df = function(x, ...) {
  x = as.data.frame(x)
  if ('limit' %in% names(list(...))) {
    record_print.data.frame(x, ...)
  } else {
    limit = getOption('xfun.md_table.limit', getOption('pillar.print_min', 10))
    record_print.data.frame(x, ..., limit = limit)
  }
}

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
    # fuse() .Rmd and mark() .md
    if (grepl('[.]Rmd$', file)) {
      if (weave) fuse(file, quiet = quiet, envir = globalenv()) else fiss(file)
    } else if (weave) mark(file) else {
      write_utf8(character(), with_ext(file, '.R'))
    }
  }
}

# filter out code from document so aspell() won't spell check code
vig_filter = function(ifile, encoding) {
  res = crack(ifile)
  res = lapply(res, function(x) {
    if (x$type == 'code_chunk') return(rep('', length(x$source)))
    if (is.character(x$source)) x$source else {
      one_string(unlist(lapply(x$source, function(s) {
        if (is.character(s)) s else ''
      })), '')
    }
  })
  structure(split_lines(unlist(res)), control = '-H -t')
}
