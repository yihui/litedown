#' A lightweight version of R Markdown
#'
#' \pkg{Markdown} is a plain-text formatting syntax that can be converted to
#' HTML and other formats. This package can render R Markdown to Markdown, and
#' then to an output document format. The main differences between this package
#' and \pkg{rmarkdown} are that it does not use Pandoc or \pkg{knitr} (i.e.,
#' fewer dependencies), and it also has fewer Markdown features.
#' @importFrom xfun base64_uri csv_options download_cache fenced_block
#'   file_exists file_ext grep_sub in_dir loadable new_record prose_index
#'   raw_string read_all read_utf8 record_print sans_ext split_lines with_ext
#'   write_utf8
'_PACKAGE'

# an internal environment to store some intermediate objects
.env = new_env()

#' @export
record_print.data.frame = function(x, ...) {
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
  x = read_utf8(ifile)
  # TODO: implement knitr:::knit_filter based on chop()
  structure(x, control = '-H -t')
}
