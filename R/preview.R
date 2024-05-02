#' Preview Markdown and R Markdown files
#'
#' Launch a web page to list and preview files under a directory.
#'
#' Markdown files will be converted to HTML and returned to the web browser
#' directly without writing to HTML files, to keep the directory clean during
#' the preview. Clicking on a filename will bring up an HTML preview. To see its
#' raw content, click on the link on its file size instead.
#' @param dir A directory path.
#' @param ... Other arguments to be passed to [xfun::new_app()].
#' @export
peek = function(dir = '.', ...) in_dir(dir, {
  xfun::new_app('litedown', lite_handler, ...)
})

# a handler returns list(payload, file, `content-type`, header, `status code`)
lite_handler = function(path, query, post, headers) {
  if (dir.exists(path)) list(payload = dir_page(path)) else {
    file_page(path, !identical(query[['preview']], '1'))
  }
}

dir_page = function(dir = '.') {
  files = list.files(dir, full.names = TRUE)
  i1 = grepl(r <- '[.]([Rq]?md|R)$', files, ignore.case = TRUE)
  # order first by folder, then by .Rmd/.R, and other files go to the end
  res = lapply(files[i1], function(f) {
    b = basename(f)
    fenced_div(c(
      fenced_div(c(
        sprintf('[%s](<%s?preview=1>)', b, b),
        sprintf('_([%s](<%s>) %s)_', file_size(f), b, file_time(f))
      ), '.name'),
      xfun::fenced_block(readLines(f, n = 10, encoding = 'UTF-8', warn = FALSE))
    ), '.box')
  })
  files = files[!i1]
  i2 = dir.exists(files)
  res = c(res, '', lapply(files[order(!i2, files)], function(f) {
    b = basename(f)
    if (d <- dir.exists(f)) b = paste0(b, '/')
    sprintf(
      '- [%s](<%s%s>)%s', b, b, if (d) '' else '?preview=1',
      if (d) '' else sprintf(' _[%s](<%s>) %s_', file_size(f), b, file_time(f))
    )
  }))
  res = c('---', '---', '', unlist(res))
  mark(res, meta = list(title = dir_title(dir), css = c('default', 'server')))
}

file_page = function(x, raw) {
  res = file_resp(x, raw)
  if (!'payload' %in% names(res)) return(res)
  # inject navigation links to the top of the page
  nav = commonmark::markdown_html(dir_title(x))
  nav = sub('<p>', '<p style="font-size: .8em;">', nav, fixed = TRUE)
  res$payload = sub('<body>', paste0('<body>\n', nav), res$payload, fixed = TRUE)
  res
}

file_resp = function(x, raw) {
  ext = if (raw) '' else tolower(xfun::file_ext(x))
  if (is.null(getOption('litedown.html.template'))) {
    opt = options(litedown.html.template = TRUE); on.exit(opt)
  }
  # TODO: support .R
  if (ext == 'md') {
    list(payload = mark(x, 'html'))
  } else if (ext %in% c('rmd', 'qmd')) {
    list(payload = fuse(x, 'html', envir = globalenv()))
  } else {
    type = xfun:::guess_type(x)
    if (!raw && (ext %in% c('js', 'latex', 'tex') || grepl('^text/', type)) &&
        !inherits(txt <- xfun::try_silent(read_utf8(x, error = TRUE)), 'try-error')) {
      list(payload = mark(fenced_block(txt, paste0('.', if (ext == '') 'plain' else ext))))
    } else {
      list(file = normalizePath(x), `content-type` = type)
    }
  }
}

file_size = function(x) xfun::format_bytes(file.size(x))
file_time = function(x) format(file.mtime(x))
dir_title = function(d) {
  links = if (d != '.') {
    if (file_exists(d)) d = dirname(d)
    d = if (d == '.') character() else unlist(strsplit(d, '/'))
    sprintf('[%s/](%s)', c('.', d), c(rev(strrep('../', seq_along(d))), './'))
  }
  one_string(c(sprintf('_%s:_', xfun::normalize_path('.')), links), ' ')
}
