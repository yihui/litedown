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
  (is_text_file(file = 'a.js'))
  (is_text_file(file = 'a.tex'))
  (is_text_file(file = 'a.xml'))
  # a .txt is text/plain via mime_type()
  (is_text_file(file = 'a.txt'))
  (!is_text_file(file = 'a.png'))
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
  (grepl('bytes|KB|B', file_size(f)))
  (is.character(file_time(f)))
})

assert('btn() builds a Markdown link with a .btn-lite class', {
  # a plain label
  (grepl('[Hi](<u>){.btn-lite}', btn('Hi', 'u'), fixed = TRUE))
  # a dotted name maps to an icon and adds the name as a class
  b = btn('.save')
  (grepl('.btn-lite .save', b, fixed = TRUE))
  (grepl(.icons[['.save']], b, fixed = TRUE))
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
  (any(grepl('<', p)))          # HTML output
  (any(grepl('index.md', p)))   # lists the files
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
  (any(grepl('<pre', p1)))
  # preview 2: fully rendered markdown (heading becomes <h1>)
  p2 = file_resp(f, '2')$payload
  (any(grepl('<h1', p2)))
})

assert('file_page() injects navigation links at the top of a rendered page', {
  d = tempfile(); dir.create(d)
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  f = file.path(d, 'a.md'); writeLines('# Hi', f)
  r = file_page(f, '1')
  (any(grepl('nav-path', r$payload)))
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
