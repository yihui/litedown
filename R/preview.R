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
#' @return A URL (invisibly) for the preview.
#' @export
roam = function(dir = '.', live = TRUE, ...) in_dir(dir, {
  # a proxy server to return files under inst/resources/
  s = xfun::new_app('.litedown', function(path, ...) {
    file_raw(pkg_file('resources', path))
  }, open = FALSE)
  # the URL needs to be translated on RStudio Server
  if (Sys.getenv('RSTUDIO_PROGRAM_MODE') == 'server' && loadable('rstudioapi'))
    s = rstudioapi::translateLocalUrl(s, TRUE)
  # load litedown assets via http://127.0.0.1:port/custom/.litedown/assets
  asset_url = function(path) paste0(s, path)

  t1 = list()  # store modification times of files
  check_time = function(path) {
    t2 = if (dir.exists(path)) {
      file.info(list.files(path, full.names = TRUE))[, 'mtime', drop = FALSE]
    } else file.mtime(path)
    t = t1[[path]]; t1[[path]] <<- t2
    res = !is.null(t) && !identical(t2, t)
    if (res) print(list(path = path, old = t, new = t2))
    res
  }

  xfun::new_app('litedown', function(path, query, post, headers) {
    # set up proper default options for mark()
    opt = options(
      litedown.html.meta = list(
        css = asset_url(c('default.css', if (dir.exists(path)) 'listing.css'))
      ),
      litedown.html.options = list(embed_resources = FALSE),
      litedown.roaming = TRUE
    )
    on.exit(options(opt), add = TRUE)
    # capture errors in fuse() because I don't know if it's possible to capture
    # general errors in the handler; without capturing errors, users will see a
    # plain-text error page, which may be hard to understand
    opts = reactor(error = TRUE); on.exit(reactor(opts), add = TRUE)
    query = as.list(query)
    # we keep POSTing to the page assets' URLs, and if an asset file has been
    # modified, we return a response telling the browser to update it
    type = if (length(post)) rawToChar(post) else ''
    # we may need to check rawToChar(headers) to decide what to do for the
    # request; for now, we simply ignore request headers, and treat the POST
    # body as the type of request
    if (type == 'open') {
      xfun:::open_path(
        query[['path']] %||% path, !dir.exists(path) && is_text_file(file = path),
        as.integer(query[['line']]) %|% -1L
      )
      return(list(payload = 'done'))
    }
    # render Rmd in new R sessions and save to file
    if (type == 'save') {
      ext = tolower(file_ext(path))
      return(if (is_lite_ext(ext)) {
        # check if the file is for a book or site
        info = proj_info(path)
        list(payload = paste('Rendered and saved:', switch(
          info$type,
          book = Rscript_call(fuse_book, list(info$root)),
          site = { Rscript_call(fuse_site, list(info$root)); info$root },
          if (ext == 'md') mark(path) else Rscript_call(fuse, list(path))
        )))
      } else {
        list(payload = paste0(
          "Unable to render '", path, "' (only ",
          paste0('.', lite_exts, collapse = ', '), " are supported)."
        ))
      })
    }
    # TODO: should we implement Hugo's --navigateToChanged?
    if (live && type != '') {
      resp = ''
      if (type %in% c('asset', 'page')) {
        if (check_time(path)) resp = '1'
      } else if (grepl('^book:', type) && check_time(f <- sub('^book:', '', type))) {
        # the book file path to preview is encoded in `type = book:foo/bar.Rmd`
        resp = fuse_book(c(dirname(path), f), 'html', globalenv())
      }
      return(list(payload = resp))
    }
    res = lite_handler(path, query, post, headers)
    # inject js to communicate with the R server via POST for live preview
    p = res$payload
    if (is.null(p) || (res[['content-type']] %||% 'text/html') != 'text/html')
      return(res)
    res$payload = sub(
      '</head>', one_string(c(
        if (live) '<meta name="live-previewer" content="litedown::roam">',
        gen_tags(asset_url(c('server.js', 'server.css'))), '</head>'
      )), p, fixed = TRUE
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
  # show file size and mtime
  info = function(f, b) {
    sprintf(
      '_( [%s](<%s>) %s%s)_', file_size(f), b, file_time(f),
      if (is_text_file(file = f)) sprintf(' [&#9998;](%s)', b) else ''
    )
  }
  i1 = grepl(r <- '[.]([Rq]?md|R)$', files, ignore.case = TRUE)
  # order first by folder, then by .Rmd/.R, and other files go to the end
  res = lapply(files[i1], function(f) {
    b = basename(f)
    fenced_div(c(
      fenced_div(c(sprintf('[%s](<%s?preview=1>)', b, b), info(f, b)), '.name'),
      xfun::fenced_block(readLines(f, n = 10, encoding = 'UTF-8', warn = FALSE))
    ), '.box')
  })
  files = files[!i1]
  i2 = dir.exists(files)
  res = c(res, '', lapply(files[order(!i2, files)], function(f) {
    b = basename(f)
    if (d <- dir.exists(f)) b = paste0(b, '/')
    sprintf(
      '- [%s](<%s%s>) %s', b, b, if (d) '' else '?preview=1',
      if (d) '' else info(f, b)
    )
  }))
  mark_full(unlist(res), meta = list(title = dir_title(dir)))
}

# add directory navigation to the top of the page
file_page = function(x, raw) {
  res = file_resp(x, raw)
  if (is.null(p <- res$payload)) return(res)
  # inject navigation links to the top of the page
  nav = commonmark::markdown_html(dir_title(x))
  nav = sub('<p>', '<p class="nav-path">', nav, fixed = TRUE)
  res$payload = sub('<body>', paste0('<body>\n', nav), p, fixed = TRUE)
  res
}

# extensions that litedown can render
lite_exts = c('md', 'rmd', 'qmd', 'r')

is_lite_ext = function(ext = file_ext(file), file) tolower(ext) %in% lite_exts

# render the path to HTML if possible
file_resp = function(x, raw) {
  ext = if (raw) '' else tolower(xfun::file_ext(x))
  if (is_lite_ext(ext)) {
    # check if the file is for a book or site
    info = proj_info(x)
    list(payload = switch(
      info$type,
      book = fuse_book(if (info$index) info$root else x, full_output, globalenv()),
      site = fuse_site(x),
      if (ext == 'md') mark_full(x) else fuse(x, full_output, envir = globalenv())
    ))
  } else {
    type = xfun:::guess_type(x)
    if (!raw && is_text_file(ext, type) &&
        !inherits(txt <- xfun::try_silent(read_utf8(x, error = TRUE)), 'try-error')) {
      list(payload = mark_full(
        fenced_block(txt, paste0('.', if (ext == '') 'plain' else ext))
      ))
    } else {
      file_raw(x, type)
    }
  }
}

# detect project type for a directory (_litedown.yml may be in an upper-level dir)
proj_info = function(x, d = dirname(x)) {
  while (length(yaml <- yml_config(d)) == 0) {
    if (xfun::same_path(d, d2 <- file.path(d, '..'))) break
    d = d2
  }
  # use the field 'type' if provided, otherwise look for 'book' or 'site'
  type = yaml[['type']] %||% head(intersect(c('book', 'site'), names(yaml)), 1)
  root = if (length(type)) d else NA
  if (is.na(root)) type = 'default'
  # a file doesn't belong to a site if it doesn't match the site file pattern
  if (type != 'default' && x != '') {
    p = yaml[[type]][['pattern']] %||% site_pattern
    if (!grepl(p, x)) type = 'default'
  }
  list(
    type = type, root = root, yaml = yaml,
    index = !is.na(root) && is_index(x) && xfun::same_path(x, file.path(root, basename(x)))
  )
}

full_output = structure('html', full = TRUE)
# generate full HTML output (instead of fragments)
mark_full = function(...) mark(..., output = full_output)

# guess if a file is a text file
is_text_file = function(ext = file_ext(file), type = xfun:::guess_type(file), file) {
  (ext %in% c('js', 'latex', 'qmd', 'tex', 'xml') || grepl('^text/', type))
}

is_roaming = function() isTRUE(getOption('litedown.roaming'))

# return a raw file response
file_raw = function(x, type = xfun:::guess_type(x)) {
  list(file = normalizePath(x), `content-type` = type)
}

file_size = function(x) xfun::format_bytes(file.size(x))
file_time = function(x) format(file.mtime(x))
dir_title = function(f) {
  links = if (f != '.') {
    d = if (file_exists(f)) dirname(f) else f
    d = if (d == '.') character() else unlist(strsplit(d, '/'))
    c(
      sprintf('[%s/](%s)', c('.', d), c(rev(strrep('../', seq_along(d))), './')),
      if (file_exists(f)) basename(f), if (is_lite_ext(file = f)) '[&#8623;](#)',
      if (is_text_file(file = f)) '[&#9998;](#)'
    )
  }
  one_string(c(sprintf('_%s:_', normalize_path('.')), links), ' ')
}
