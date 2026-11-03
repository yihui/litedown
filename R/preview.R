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
  s = new_app('.litedown', function(path, ...) {
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
    !is.null(t) && !(is.null(dim(t2)) && is.na(t2)) && !identical(t2, t)
  }

  new_app('litedown', function(path, query, post, headers) {
    # set up proper default options for mark()
    opt = options(
      litedown.html.meta = list(
        css = asset_url(c('default.css', if (dir.exists(path)) 'listing.css'))
      ),
      litedown.html.options = list(embed_resources = FALSE),
      litedown.roaming = TRUE
    )
    on.exit(options(opt), add = TRUE)
    query = as.list(query)
    ext = tolower(file_ext(path))
    # we keep POSTing to the page assets' URLs, and if an asset file has been
    # modified, we return a response telling the browser to update it; the
    # request headers tell us what to do for the request, and the POST body is
    # ignored
    type = req_header(headers, 'litedown-data')
    # a heartbeat, so the browser can tell if the server is alive
    if (type == 'ping') return(list(payload = ''))
    if (type == 'open') {
      xfun:::open_path(
        query[['path']] %||% path, !dir.exists(path) && is_text_file(file = path),
        as.integer(query[['line']]) %|% -1L
      )
      return(list(payload = 'done'))
    }
    # preview the YAML of a new file with selected features
    if (grepl('^yaml:', type))
      return(list(payload = one_string(new_yaml(feature_list(type)))))
    # create a new file with selected features
    if (grepl('^new($|:)', type))
      return(list(payload = if (type == 'new') feature_form(path) else new_file(path, ext, type)))
    # render Rmd in new R sessions and save to file
    if (type == 'save') {
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
    # show errors on a page (instead of a plain-text error) so that the page
    # keeps being live and recovers once the error is fixed
    res = catch_page(
      lite_handler(path, query, post, headers), asset_url('default.css')
    )
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

# get the value of a request header (case-insensitive); '' if not found
req_header = function(headers, name) {
  if (!length(headers)) return('')
  x = grep_sub(
    sprintf('.*\n%s: ([^[:space:]]+).*', name), '\\1', rawToChar(headers),
    ignore.case = TRUE
  )
  if (length(x) == 1) x else ''
}

# evaluate an expression that generates a page, and return an error page with a
# traceback if it fails
catch_page = function(expr, css) {
  steps = NULL
  tryCatch(
    withCallingHandlers(expr, error = function(e) {
      calls = sys.calls()
      # only keep the calls evaluated by `expr`, i.e., after withCallingHandlers()
      i = max(which(vapply(calls, is_call, NA, 'withCallingHandlers')))
      # exclude the two frames of the error handler itself (h() and this function)
      k = seq_len(length(calls) - 2L)[-seq_len(i)]
      # drop calls of package functions and eval() (not user code)
      k = k[!vapply(k, function(j) {
        is_call(calls[[j]], c('eval', 'evalq')) || isNamespace(environment(sys.function(j)))
      }, NA)]
      # steps innermost first
      steps <<- lapply(rev(k), function(j) step_info(calls[[j]]))
      # the error occurs in the innermost function (not a call frame itself), so
      # link to where it is defined; if no function is involved, it is a line of
      # the code chunk (top-level expression)
      if (length(k) && !is.null(cl <- conditionCall(e))) {
        r = attr(sys.function(max(k)), 'srcref')
        steps <<- c(list(step_info(cl, r)), steps)
      }
      else steps <<- c(steps, dropNULL(list(chunk_step())))
    }),
    error = function(e) list(
      payload = error_page(css, e, steps), `status code` = 500L
    )
  )
}

is_call = function(x, f) is.symbol(x[[1]]) && as.character(x[[1]]) %in% f

# the call (or a string) of a traceback step and the source location (if available) as a
# list(call, file, line); file is NULL if there is no source reference
step_info = function(x, r = attr(x, 'srcref')) {
  f = l = NULL
  if (inherits(r, 'srcref') && !is.null(sf <- attr(r, 'srcfile'))) {
    f = sf$filename
    if (!is_abs_path(f)) f = file.path(sf$wd %||% '.', f)
    if (file_exists(f)) {
      f = normalizePath(f); l = r[1]
    } else f = NULL
  }
  if (!is.character(x)) x = paste(deparse(x, width.cutoff = 60L)[1], collapse = '')
  list(call = x, file = f, line = l)
}

# if an error occurs in a top-level expression of a code chunk in fuse(), the
# source line of the expression as a traceback step (NULL if not in a chunk)
chunk_step = function() {
  # xfun::record() parses the code with source references (the `src` argument),
  # and `expr` in its frame is the expression being evaluated
  for (j in rev(seq_len(sys.nframe()))) {
    if (!identical(sys.function(j), xfun::record)) next
    # TODO: no source references with xfun < 0.62.8; remove this check later
    if (is.null(r <- attr(get0('expr', sys.frame(j), inherits = FALSE), 'srcref'))) return()
    s = step_info(trimws(as.character(r[[1]])[1]), r[[1]])
    return(if (!is.null(s$file)) s)
  }
}

# an HTML page that displays an error when previewing a file, with the traceback
# of source steps (each step is list(call, file, line))
error_page = function(css, e, steps = list()) {
  msg = if (is.character(e)) e else conditionMessage(e)
  cl = if (!is.character(e) && !is.null(cl <- conditionCall(e)))
    paste(deparse(cl, width.cutoff = 60L)[1], collapse = '')
  li = vapply(steps, function(s) sprintf(
    '<li%s><code>%s</code>%s</li>', if (is.null(s$file)) ' class="muted"' else '',
    html_escape(substr(s$call, 1, 100)), if (is.null(s$file)) '' else sprintf(
      ' <a href="?path=%s&amp;line=%d" title="Open %s in editor">%s#%d</a>',
      URLencode(s$file, TRUE), s$line, html_escape(s$file, TRUE),
      html_escape(basename(s$file)), s$line
    )
  ), '')
  one_string(c(
    '<!DOCTYPE html><html><head><meta charset="utf-8"><title>Error</title>',
    sprintf('<link rel="stylesheet" href="%s"></head><body><main>', css),
    '<p><a href="javascript:history.back()" class="btn-lite">&larr; Back</a></p>',
    '<h3>Failed to preview the file</h3>',
    # no line breaks between the tags and the message (else <pre> shows them)
    paste0(
      '<pre><code class="error">',
      one_string(html_escape(c(if (!is.null(cl)) paste('Error in', cl), msg))),
      '</code></pre>'
    ),
    if (length(li)) c(
      sprintf('<details open><summary>Traceback (%d, innermost first)</summary>', length(li)),
      '<ol class="traceback">', li, '</ol></details>'
    ), '</main></body></html>'
  ), '\n')
}

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
  # show file size and mtime (as relative time, which is computed in JS)
  info = function(f, b) {
    t = file.mtime(f)
    sprintf(
      '_( [%s](<%s>){title="Raw file"} <time datetime="%s" data-ms="%.0f" title="%s">%s</time>)_',
      file_size(f), b, format(t, '%Y-%m-%dT%H:%M:%S%z'), as.numeric(t) * 1000,
      format(t), format(t)
    )
  }
  # create link to preview a file
  p_link = function(f, t = f, n = 1, a = NULL) {
    btn(t, sprintf('%s?preview=%d', f, n), a)
  }
  # files starting with _ or . are dimmed
  muted = function(f) if (grepl('^[_.]', basename(f))) '.muted' else NULL
  i1 = is_lite_ext(file = files)
  # a .md file is not a box if a .Rmd/.qmd file of the same name exists (it is
  # listed as a plain file below), same as find_input()
  i1 = i1 & !(
    tolower(file_ext(files)) == 'md' &
      sans_ext(basename(files)) %in% sub('[.][Rq]md$', '', basename(files))
  )
  # order first by folder, then by .Rmd/.R, and other files go to the end
  boxes = lapply(files[i1][order(grepl('^[_.]', basename(files[i1])))], function(f) {
    b = basename(f)
    txt = readLines(f, n = 10, encoding = 'UTF-8', warn = FALSE)
    fenced_div(c(
      fenced_div(c(
        p_link(b, a = NULL), info(f, b),
        p_link(b, '.run', 2, 'title="Run in memory"')
      ), '.caption .name'),
      fenced_block(txt)
    ), c('.box', muted(f)))
  })
  # the boxes are laid out in a grid (see listing.css)
  res = if (length(boxes)) fenced_div(unlist(boxes), '.boxes')
  files = files[!i1]
  i2 = dir.exists(files)
  items = lapply(files[order(!i2, files)], function(f) {
    b = basename(f)
    if (d <- dir.exists(f)) b = paste0(b, '/')
    sprintf(
      '- [%s](<%s%s>)%s %s', b, b, if (d) '' else '?preview=1',
      if (length(m <- muted(f))) paste0('{', m, '}') else '',
      if (d) '' else info(f, b)
    )
  })
  # the file list is styled by listing.css (`.listing`)
  if (length(items)) res = c(res, '', fenced_div(unlist(items), '.listing'))
  tools = if (length(list.files(dir)) > 4) list_tools()
  mark_full(c(tools, unlist(res)), meta = list(title = dir_title(dir)))
}

# a filter box and a sorting menu for the file listing
list_tools = function() c(
  '<div class="listing-tools">',
  '<input type="search" placeholder="Filter files (press / to focus)" aria-label="Filter files">',
  '<label>Sort by <select><option value="">default</option><option value="name">name</option>',
  '<option value="time">modified time</option></select></label></div>', ''
)

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
  ext = if (raw) '' else tolower(file_ext(x))
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
        store_book(info$root, x, info$index)
        fuse_book(if (info$index) info$root else x, full_output, globalenv())
      },
      site = fuse_site(x),
      if (ext == 'md') mark_full(x) else fuse(x, full_output, envir = globalenv())
    ))
  } else {
    type = mime_type(x)
    if (!raw && is_text_file(ext, type) &&
        !inherits(txt <- try_silent(read_utf8(x, error = TRUE)), 'try-error')) {
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
store_book = function(dir, x, index = FALSE) {
  f = function(...) .mapply(
    function(n, v) .env[[paste0('current_', n)]] = v,
    c('book', 'file', 'index'), list(...)
  )
  f(dir, x, index)
  exit_call(function() f(NULL))
}

# detect project type for a directory (_litedown.yml may be in an upper-level dir)
proj_info = function(x, d = dirname(x)) {
  while (length(yaml <- yml_config(d)) == 0) {
    if (same_path(d, d2 <- file.path(d, '..'))) break
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
    index = !is.na(root) && is_index(x) && same_path(x, file.path(root, basename(x)))
  )
}

full_output = structure('html', full = TRUE)
# generate full HTML output (instead of fragments)
mark_full = function(...) mark(..., output = full_output)

# guess if a file is a text file
is_text_file = function(ext = file_ext(file), type = mime_type(file), file) {
  (ext %in% c('js', 'latex', 'qmd', 'tex', 'xml') || grepl('^text/', type))
}

is_html_file = function(x) {
  file_exists(x) && tolower(file_ext(x)) %in% c('html', 'htm')
}

is_roaming = function() isTRUE(getOption('litedown.roaming'))

# return a raw file response
file_raw = function(x, type = mime_type(x)) {
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
      ) else if (is_html_file(f))
        btn('.run', b, 'title="View the HTML page"'),
      if (file_exists(f) && is_text_file(file = f)) btn('.open')
    )
  }
  # show the folder name only; the full path is in the tooltip
  w = normalize_path('.')
  txt = one_string(c(
    sprintf('_<span title="%s">%s:</span>_', html_escape(w, TRUE), html_escape(basename(w))),
    links
  ), ' ')
  markdown_html(txt, smart = TRUE)
}

btn = function(t, u = '#', a = character()) {
  if (startsWith(t, '.')) {
    # default tooltips for icons (unless provided)
    if (!any(grepl('title=', a))) a = c(a, sprintf('title="%s"', .titles[t]))
    a = c(t, a); t = .icons[t]
  }
  a = if (is.character(a)) one_string(c('.btn-lite', a), ' ')
  a = if (is.null(a)) '' else paste0('{', a, '}')
  sprintf(' [%s](<%s>)%s ', t, u, a)
}

.icons = c(.open = '&#9998;', .run = '&#9658;', .save = '&#8623;')
.titles = c(
  .open = 'Open in editor', .run = 'Run in memory and preview',
  .save = 'Render to disk in a fresh R session'
)

# features selected in a request of the type `new:a,b` or `yaml:a,b`
feature_list = function(type) {
  x = strsplit(sub('^[a-z]+:', '', type), ',')[[1]]
  # in the order of assets (e.g., default first), not the order of the request
  intersect(rownames(assets), x)
}

# YAML frontmatter for a new file with selected features
new_yaml = function(features = NULL) {
  a = assets[features, , drop = FALSE]
  m = list(
    title = 'Comfortably Untitled',
    author = tools::toTitleCase(Sys.getenv('USERNAME', Sys.getenv('USER'))),
    date = format(Sys.Date())
  )
  if (nrow(a)) m$output$html = list(meta = list(
    css = I(na_omit(a[, 'css'])), js = I(na_omit(a[, 'js']))
  ))
  c('---', xfun::taml_save(m), '---')
}

new_file = function(path, ext, type) {
  if (file.exists(path)) return(sprintf("The file '%s' already exists.", path))
  on.exit(xfun:::open_path(path), add = TRUE)
  if (!is_lite_ext(ext)) return(tolower(file.create(path)))
  txt = c(new_yaml(feature_list(type)), '', 'Relax. I need some information first.')
  if (ext == 'r') txt = paste("#'", split_lines(txt))
  write_utf8(txt, path)
  'view'
}
