library(testit)

assert('is_lite_ext() recognizes extensions litedown can render', {
  (is_lite_ext(file = 'a.Rmd'))
  (is_lite_ext(file = 'a.md'))
  (is_lite_ext(file = 'a.MD'))   # case-insensitive
  (is_lite_ext(file = 'a.qmd'))
  (is_lite_ext(file = 'a.r'))
  (!is_lite_ext(file = 'a.png'))
})

assert('is_text_file() detects text files by extension or MIME type', {
  # the extension branch (js/tex/xml/...) doesn't need the file to exist
  (is_text_file(file = 'a.js'))
  (is_text_file(file = 'a.tex'))
  (is_text_file(file = 'a.xml'))
  # otherwise it falls back to the MIME type
  (is_text_file('txt', 'text/plain', 'a.txt'))
  (!is_text_file('png', 'image/png', 'a.png'))
})

assert('is_roaming() reflects the litedown.roaming option', {
  (!is_roaming())
  opt = options(litedown.roaming = TRUE)
  (is_roaming())
  options(opt)
  (!is_roaming())
})

assert('file_raw() returns a raw file response with a content type', {
  f = tempfile(fileext = '.txt'); writeLines('x', f)
  on.exit(unlink(f), add = TRUE)
  r = file_raw(f)
  (same_path(r$file, f))
  (r[['content-type']] %==% 'text/plain')
})

assert('file_size() and file_time() format file metadata', {
  f = tempfile(); writeLines('hello', f)
  on.exit(unlink(f), add = TRUE)
  (has_match(file_size(f), 'bytes|KB|B'))
  (is.character(file_time(f)))
})

assert('btn() builds a Markdown link with a .btn-lite class', {
  # a plain label
  (has_match(btn('Hi', 'u'), I('[Hi](<u>){.btn-lite}')))
  # a dotted name maps to an icon and adds the name as a class
  b = btn('.save')
  (has_match(b, I('.btn-lite .save')))
  (has_match(b, I(.icons[['.save']])))
})

assert('proj_info() classifies a directory as default, book, or site', {
  d = tempfile(); dir.create(d)
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  f = file.path(d, 'a.md'); writeLines('x', f)
  # no config -> default project, no root
  pi = proj_info(f)
  (pi$type %==% 'default')
  (is.na(pi$root))
  # a _litedown.yml with a 'book' field -> book project
  writeLines(c('book:', '  new_session: false'), file.path(d, '_litedown.yml'))
  pi2 = proj_info(file.path(d, 'index.md'))
  (pi2$type %==% 'book')
  (isTRUE(pi2$index))         # index.md at the root is the book index
  (!is.na(pi2$root))
})

assert('dir_page() renders a directory listing to HTML', {
  d = tempfile(); dir.create(d)
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  writeLines('# Hello', file.path(d, 'index.md'))
  writeLines('x', file.path(d, 'notes.txt'))
  p = as.character(dir_page(d))
  (has_match(p, '<'))          # HTML output
  (has_match(p, 'index.md'))   # lists the files
  # plain files are in a .listing block, which scopes listing.css
  (has_match(p, '<div class="listing">\n<ul>'))
})

assert('file_resp() sends raw, verbatim, or rendered responses by preview level', {
  d = tempfile(); dir.create(d)
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  f = file.path(d, 'a.md'); writeLines('# Hi', f)
  # preview 0: raw file response
  (!is.null(file_resp(f, '0')$file))
  # preview 1: verbatim (source shown in a code block), rendered to HTML
  p1 = file_resp(f, '1')$payload
  (!is.null(p1))
  (has_match(p1, '<pre'))
  # preview 2: fully rendered markdown (heading becomes <h1>)
  p2 = file_resp(f, '2')$payload
  (has_match(p2, '<h1'))
})

assert('file_page() injects navigation links at the top of a rendered page', {
  d = tempfile(); dir.create(d)
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  f = file.path(d, 'a.md'); writeLines('# Hi', f)
  r = file_page(f, '1')
  (has_match(r$payload, 'nav-path'))
})

assert('directory pages use .nav-path (not an article .title) as the nav bar', {
  d = tempfile(); dir.create(d)
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  p = lite_handler(d, list(), NULL, NULL)$payload
  (has_match(p, '<body>\n<p class="nav-path">'))
  (has_match(p, !'class="title"'))
})

assert('lite_handler() dispatches directories and files', {
  d = tempfile(); dir.create(d)
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  writeLines('# Hi', file.path(d, 'a.md'))
  # a directory path yields a directory listing
  (!is.null(lite_handler(d, list(), NULL, NULL)$payload))
  # a file path yields a file response
  (!is.null(lite_handler(file.path(d, 'a.md'), list(preview = '1'), NULL, NULL)$payload))
})

assert('req_header() extracts request headers case-insensitively', {
  h = charToRaw('GET / HTTP/1.1\nLitedown-Data: ping\nHost: x\n')
  (req_header(h, 'litedown-data') %==% 'ping')
  (req_header(h, 'nothing') %==% '')
  (req_header(NULL, 'litedown-data') %==% '')
})

assert('error_page() escapes the message and links the CSS', {
  p = error_page('a.css', '<b>oops</b>')
  (has_match(p, 'href="a.css"'))
  (has_match(p, '&lt;b&gt;oops'))
})

assert('error_page() starts the error box with the message, without blank lines', {
  p = error_page('a.css', simpleError('oops', quote(f(x))))
  (has_match(p, '<code class="error">Error in f\\(x\\)\noops</code>'))
})

assert('catch_page() returns an error page with a traceback and source links', {
  f = tempfile(fileext = '.R')
  writeLines(c('g = function() stop("boom")', 'h = function() {', '  g()', '}'), f)
  on.exit(unlink(f), add = TRUE)
  env = new.env()
  sys.source(f, env, keep.source = TRUE)
  r = catch_page(env$h(), 'a.css')
  (r[['status code']] %==% 500L)
  (has_match(r$payload, 'boom'))
  (has_match(r$payload, 'Traceback'))
  # links to the lines in the source file (innermost call first)
  (has_match(r$payload, 'line=3'))
  (has_match(r$payload, sprintf('%s#3', basename(f))))
  # no error: the value of the expression
  (catch_page(1 + 1, 'a.css') %==% 2)
  # errors without source references still have a traceback, but no links
  r2 = catch_page(stop('no srcref'), 'a.css')
  (has_match(r2$payload, 'no srcref'))
  (has_match(r2$payload, !I('?path=')))
})

assert('btn() adds default tooltips to icon buttons', {
  (has_match(btn('.run'), 'title="Run in memory'))
  (has_match(btn('.run', a = 'title="Run"'), 'title="Run"'))
})

assert('new_yaml() and feature_list() build YAML for selected features', {
  (feature_list('new:snap,bogus,tabsets') %==% c('snap', 'tabsets'))
  (feature_list('yaml:') %==% character())
  # default CSS must come first, no matter in which order features are requested
  (feature_list('new:tabsets,snap,default,article') %==%
    c('default', 'article', 'snap', 'tabsets'))
  (rownames(assets)[1] %==% 'default')
  y = new_yaml(feature_list('new:snap-clean,snap,default'))
  (has_match(y, 'css: \\["@default", "@snap", "@snap-clean"\\]'))
  y = new_yaml('tabsets')
  (y[1] %==% '---' && y[length(y)] %==% '---')
  (has_match(y, '@tabsets'))
  (has_match(new_yaml(), !'output'))
  # all presets only use known features
  (all(unlist(feature_presets) %in% rownames(assets)))
})

assert('feature_form() includes the filename hint, presets, and YAML preview', {
  f = feature_form('.')
  (has_match(f, 'id="filename-hint"'))
  (has_match(f, 'data-features='))
  (has_match(f, 'id="yaml-preview"'))
})

assert('dir_page() shows relative-time data, and dims files starting with _', {
  d = tempfile(); dir.create(d)
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  writeLines(c('---', 'title: "My <Doc>"', '---', 'x'), file.path(d, 'a.Rmd'))
  writeLines('x', file.path(d, '_b.md'))
  for (i in 1:5) writeLines('x', file.path(d, sprintf('f%d.txt', i)))
  p = paste(as.character(dir_page(d)), collapse = '\n')
  # the title is shown once (in the content of the box), not in the box title
  (length(gregexpr('My &lt;Doc&gt;', p)[[1]]) %==% 1L)
  (has_match(p, 'data-ms='))
  (has_match(p, 'class="box muted"'))
  (has_match(p, 'class="boxes"'))
  (has_match(p, 'class="listing-tools"'))
})

# TODO: remove the condition after requiring xfun >= 0.62.8 (`src` in record())
if ('src' %in% names(formals(xfun::record))) assert('catch_page() links tracebacks to source lines of code chunks in fuse()', {
  rmd = tempfile(fileext = '.Rmd')
  on.exit(unlink(rmd), add = TRUE)
  # fuse() the file and return the traceback as (call, link text) pairs
  trace = function(lines) {
    writeLines(lines, rmd)
    r = suppressMessages(catch_page(fuse(rmd, 'markdown', quiet = TRUE), 'a.css'))
    x = regmatches(r$payload, gregexpr('<li><code>.*?</li>', r$payload))[[1]]
    list(
      status = r[['status code']],
      call = sub('<li><code>(.*?)</code>.*', '\\1', x),
      link = sub('.*>(.*?)</a></li>', '\\1', x)
    )
  }
  # fuse() assigns chunk objects to the global env, so avoid the names f and g
  # innermost first: the failing function (where it is defined), the call of
  # g() in f(), and the call of f() in the chunk
  x = trace(c(
    'Text', '', '```{r}', 'g = function() stop("bad")', 'f = function() g()',
    'f()', '```'
  ))
  (x$status %==% 500L)
  (x$call %==% c('g()', 'g()', 'f()'))
  (x$link %==% paste0(basename(rmd), c('#4', '#5', '#6')))
  # an error in a top-level expression of the second chunk, after the pipe
  # comments and other chunks, is located by the line in the file
  x = trace(c(
    '```{r}', 'x = 1', '```', '', 'Text', '', '```{r}', '#| echo: false',
    '#| label: second', 'y = 2', 'stop(1)', '```'
  ))
  (x$status %==% 500L)
  (x$call %==% 'stop(1)')
  (x$link %==% paste0(basename(rmd), '#11'))
})

assert('listing pages only have Run buttons, while file pages have edit and render buttons', {
  d = tempfile(); dir.create(d)
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  for (f in c('a.Rmd', 'b.md', 'c.txt')) writeLines('x', file.path(d, f))
  # icons in the rendered HTML are characters instead of entities
  ico = function(x) intToUtf8(as.integer(gsub('\\D', '', .icons[[x]])))
  p = paste(as.character(dir_page(d)), collapse = '\n')
  # one Run button for each of the two documents, none for the text file
  (lengths(gregexpr(ico('.run'), p, fixed = TRUE)) %==% 2L)
  (has_match(p, !ico('.open')))
  (has_match(p, !ico('.save')))
  nav = file_page(file.path(d, 'a.Rmd'), '1')$payload
  (has_match(nav, ico('.open')))
  (has_match(nav, ico('.save')))
})

assert('dir_page() lists foo.md as a plain file when foo.Rmd exists', {
  d = tempfile(); dir.create(d)
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  for (f in c('foo.Rmd', 'foo.md', 'bar.md')) writeLines('x', file.path(d, f))
  p = paste(as.character(dir_page(d)), collapse = '\n')
  (length(gregexpr('class="box"', p)[[1]]) %==% 2L)
  (has_match(p, '<li><a href="foo.md\\?preview=1"'))
})
