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
    !is.null(t) && !identical(t2, t)
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
    type = if (length(headers)) grep_sub(
      '.*\nlitedown-data: ([^[:space:]]+).*', '\\1', rawToChar(headers)
    )
    if (length(type) != 1) type = ''
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
    # clean up __files/
    if (type == 'cleanup') {
      if (dir.exists(fig <- fig_path(path)) &&
          !is.null(fig_time <- .env$roam_files[[fig]]) &&
          !dir.exists(aux_path(, 'cache.path', path))) {
        fig_time2 = file.mtime(fig)
        # remove fig dir if it has not been modified since its mtime was
        # recorded last time (in file_resp()); if it has, update to new mtime so
        # we can clean it up next time when we receive the request; checking
        # mtime is necessary because the Ajax cleanup request (sent from the
        # 'beforeunload' event) may arrive _after_ file_resp() rebuilds the
        # current page when the page is refreshed, which may be counterintuitive
        # but Ajax is _asynchronous_ anyway (this took me hours to figure out)
        .env$roam_files[[fig]] = if (fig_time2 <= fig_time) {
          unlink(fig, recursive = TRUE); NULL
        } else fig_time2
      }
      return(list(payload = ''))
    }
    # TODO: should we implement Hugo's --navigateToChanged?
    if (live && type != '') {
      resp = ''
      if (type %in% c('asset', 'page')) {
        if (check_time(path)) resp = '1'
      } else if (grepl('^book:', type) && check_time(f <- sub('^book:', '', type))) {
        store_book(dirname(path), f)
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
    p = sub(
      '</head>', one_string(c(
        if (live) '<meta name="live-previewer" content="litedown::roam">',
        gen_tags(asset_url('server.css')), '</head>'
      )), p, fixed = TRUE
    )
    p = sub(
      '</body>', one_string(c(gen_tags(asset_url('server.js')), '</body>')), p,
      fixed = TRUE
    )
    res$payload = p
    res
  }, ...)
})

# a handler returns list(payload, file, `content-type`, header, `status code`)
lite_handler = function(path, query, post, headers) {
  if (dir.exists(path)) list(payload = dir_page(path)) else {
    file_page(path, query[['preview']] %||% '0')
  }
}

dir_page = function(dir = '.') {
  files = list.files(dir, full.names = TRUE)
  # index.* files should appear first
  files = files[order(!sans_ext(basename(files)) == 'index')]
  # show file size and mtime
  info = function(f, b, extra = '') {
    sprintf(
      '_( [%s](<%s>){title="Raw file"} %s%s%s)_', file_size(f), b, file_time(f),
      if (is_text_file(file = f)) btn('.open', b) else '', extra
    )
  }
  # create link to preview a file
  p_link = function(f, t = f, n = 1, a = NULL) {
    btn(t, sprintf('%s?preview=%d', f, n), a)
  }
  i1 = is_lite_ext(file = files)
  # order first by folder, then by .Rmd/.R, and other files go to the end
  res = lapply(files[i1], function(f) {
    b = basename(f)
    fenced_div(c(
      fenced_div(c(
        p_link(b, a = NULL), info(f, b, btn('.save', b)),
        p_link(b, '.run', 2, 'title="Run in memory"')
      ), '.caption .name'),
      fenced_block(readLines(f, n = 10, encoding = 'UTF-8', warn = FALSE))
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
file_page = function(x, preview) {
  res = file_resp(x, preview)
  if (is.null(p <- res$payload)) return(res)
  # inject navigation links to the top of the page
  nav = dir_title(x, preview)
  nav = sub('<p>', '<p class="nav-path">', nav, fixed = TRUE)
  res$payload = sub('<body>', paste0('<body>\n', nav), p, fixed = TRUE)
  res
}

# extensions that litedown can render
lite_exts = c('md', 'rmd', 'qmd', 'r')

is_lite_ext = function(ext = file_ext(file), file) tolower(ext) %in% lite_exts

# render the path to HTML if possible
file_resp = function(x, preview) {
  raw = preview == '0'  # 0: send raw response; 1: render verbatim; 2: fuse()/mark()
  ext = if (raw) '' else tolower(xfun::file_ext(x))
  if (preview == '2' && is_lite_ext(ext)) {
    # to clean up the __files/ dir if requested (via options()) and the dir
    # didn't exist before
    if (getOption('litedown.roam.cleanup', FALSE)) {
      fig = fig_path(x)
      if (!dir.exists(fig)) on.exit({
        if (dir.exists(fig)) .env$roam_files[[fig]] = file.mtime(fig)
      })
    }
    # check if the file is for a book or site
    info = proj_info(x)
    list(payload = switch(
      info$type,
      book = {
        store_book(info$root, x)
        fuse_book(if (info$index) info$root else x, full_output, globalenv())
      },
      site = fuse_site(x),
      if (ext == 'md') mark_full(x) else fuse(x, full_output, envir = globalenv())
    ))
  } else {
    type = xfun::mime_type(x)
    if (!raw && is_text_file(ext, type) &&
        !inherits(txt <- xfun::try_silent(read_utf8(x, error = TRUE)), 'try-error')) {
      list(payload = mark_full(
        fenced_block(txt, lineno_attr(if (ext == '') 'plain' else ext))
      ))
    } else {
      file_raw(x, type)
    }
  }
}

# store book dir for books to resolve number_refs() because the book may be
# partially rendered (in which case we can't resolve refs to other chapters)
store_book = function(dir, x) {
  .env$current_book = dir; .env$current_file = x
  xfun::exit_call(function() .env$current_book <- .env$current_file <- NULL)
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
is_text_file = function(ext = file_ext(file), type = xfun::mime_type(file), file) {
  (ext %in% c('js', 'latex', 'qmd', 'tex', 'xml') || grepl('^text/', type))
}

is_roaming = function() isTRUE(getOption('litedown.roaming'))

# return a raw file response
file_raw = function(x, type = xfun::mime_type(x)) {
  list(file = normalizePath(x), `content-type` = type)
}

file_size = function(x) xfun::format_bytes(file.size(x))
file_time = function(x) format(file.mtime(x))
dir_title = function(f, preview = '1') {
  links = if (f != '.') {
    d = if (file_exists(f)) dirname(f) else f
    d = if (d == '.') character() else unlist(strsplit(d, '/'))
    b = basename(f)
    c(
      sprintf('[%s/](%s)', c('.', d), c(rev(strrep('../', seq_along(d))), './')),
      if (file_exists(f)) b, if (is_lite_ext(file = f)) c(
        btn('.save'), if (preview != '2')
          btn('.run', sprintf('%s?preview=2', b), 'title="Run"')
      ),
      if (is_text_file(file = f)) btn('.open')
    )
  }
  txt = one_string(c(sprintf('_%s:_', normalize_path('.')), links), ' ')
  move_attrs(commonmark::markdown_html(txt, smart = TRUE))
}

btn = function(t, u = '#', a = character()) {
  if (startsWith(t, '.')) {
    a = c(t, a); t = .icons[t]
  }
  a = if (is.character(a)) one_string(c('.btn-lite', a), ' ')
  a = if (is.null(a)) '' else paste0('{', a, '}')
  sprintf(' [%s](<%s>)%s ', t, u, a)
}

.icons = c(.open = '&#9998;', .run = '&#9205;', .save = '&#8623;')
