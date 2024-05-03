#' Preview Markdown and R Markdown files
#'
#' Launch a web page to list and preview files under a directory.
#'
#' Markdown files will be converted to HTML and returned to the web browser
#' directly without writing to HTML files, to keep the directory clean during
#' the preview. Clicking on a filename will bring up an HTML preview. To see its
#' raw content, click on the link on its file size instead.
#' @param dir A directory path.
#' @param live Whether to enable live preview. If enabled, the browser page will
#'   be automatically updated upon modification of local files used by the page
#'   (e.g., the Markdown file or external CSS/JS/image files). If disabled, you
#'   can manually refresh the page to fully re-render it.
#' @param ... Other arguments to be passed to [xfun::new_app()].
#' @export
peek = function(dir = '.', live = TRUE, ...) in_dir(dir, {
  # a proxy server to return files under inst/resources/
  s = xfun::new_app('.litedown', function(path, ...) {
    file_raw(pkg_file('resources', path))
  }, open = FALSE)
  # load litedown assets via http://127.0.0.1:port/custom/.litedown/assets
  asset_url = function(path) paste0(s, path)

  t1 = list()  # store modification times of files
  check_time = function(path) {
    !is.na(t2 <- file.mtime(path)) && {
      t = t1[[path]]; t1[[path]] <<- t2
      (!is.null(t) && t2 > t)
    }
  }

  xfun::new_app('litedown', function(path, query, post, headers) {
    # set up proper default options for mark()
    opt = options(
      litedown.html.template = TRUE,
      litedown.html.meta = list(
        css = asset_url(c('default.css', if (dir.exists(path)) 'server.css'))
      ),
      litedown.html.options = list(embed_resources = FALSE)
    )
    on.exit(options(opt), add = TRUE)
    # we keep POSTing to the page assets' URLs, and if an asset file has been
    # modified, we return a response telling the browser to update it
    if (live && length(post)) {
      # we may need to check rawToChar(headers) to decide what to do for the
      # request; for now, we simply ignore request headers
      type = rawToChar(post)  # the POST body is the type of request
      resp = ''
      if (type == 'asset') {
        if (check_time(path)) resp = '1'
      } else if (type == 'page') {
        if (check_time(path)) resp = '1'
      } else if (type == 'book') {
        if (check_time(path)) resp = ''
      }
      return(list(payload = resp))
    }
    res = lite_handler(path, as.list(query), post, headers)
    # inject js to communicate with the R server via POST for live preview
    p = res$payload
    if (!live || is.null(p) || (res[['content-type']] %||% 'text/html') != 'text/html')
      return(res)
    res$payload = sub(
      '</head>', paste0(gen_tag(asset_url('server.js')), '</head>'), p,
      fixed = TRUE
    )
    res
  }, ...)
})

# a handler returns list(payload, file, `content-type`, header, `status code`)
lite_handler = function(path, query, post, headers) {
  if (dir.exists(path)) list(payload = dir_page(path)) else {
    file_page(path, !identical(query[['preview']], '1'))
  }
}

dir_page = function(dir = '.') {
  files = list.files(dir, full.names = TRUE)
  # index.* files should appear first
  files = files[order(!sans_ext(basename(files)) == 'index')]
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
  mark(unlist(res), meta = list(title = dir_title(dir)))
}

# add directory navigation to the top of the page
file_page = function(x, raw) {
  res = file_resp(x, raw)
  if (is.null(p <- res$payload)) return(res)
  # inject navigation links to the top of the page
  nav = commonmark::markdown_html(dir_title(x))
  nav = sub('<p>', '<p style="font-size: .8em;">', nav, fixed = TRUE)
  res$payload = sub('<body>', paste0('<body>\n', nav), p, fixed = TRUE)
  res
}

# render the path to HTML if possible
file_resp = function(x, raw) {
  ext = if (raw) '' else tolower(xfun::file_ext(x))
  # TODO: support .R
  if (ext == 'md') {
    list(payload = mark(x, 'html'))
  } else if (ext %in% c('rmd', 'qmd')) {
    # check if the file is for a book
    txt = read_utf8(x)
    yaml = yaml_body(txt)$yaml
    list(payload = if ('book' %in% names(yaml[['litedown']])) {
      fuse_book(dirname(x), 'html', globalenv())
    } else {
      fuse(x, 'html', txt, envir = globalenv())
    })
  } else {
    type = xfun:::guess_type(x)
    if (!raw && (ext %in% c('js', 'latex', 'tex') || grepl('^text/', type)) &&
        !inherits(txt <- xfun::try_silent(read_utf8(x, error = TRUE)), 'try-error')) {
      list(payload = mark(fenced_block(txt, paste0('.', if (ext == '') 'plain' else ext))))
    } else {
      file_raw(x, type)
    }
  }
}

# return a raw file response
file_raw = function(x, type = xfun:::guess_type(x)) {
  list(file = normalizePath(x), `content-type` = type)
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
