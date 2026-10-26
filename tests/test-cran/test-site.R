library(testit)

assert('menu_name() turns a filename stem into a title', {
  (menu_name('about-us') %==% 'About Us')
  (menu_name('my_page') %==% 'My Page')
})

assert('is_index() detects an index file regardless of directory/extension', {
  (is_index('index.Rmd'))
  (is_index('a/b/index.md'))
  (!is_index('a/foo.md'))
})

assert('reorder_input() puts the index file first', {
  (basename(reorder_input(c('a/b.md', 'a/index.md'))) %==% c('index.md', 'b.md'))
  # the shortest index path wins when several exist
  x = reorder_input(c('sub/index.md', 'index.md', 'z.md'))
  (basename(x[1]) %==% 'index.md')
  (x[1] %==% 'index.md')
})

assert('find_input() lists input files, excluding hidden/underscore/readme', {
  d = tempfile(); dir.create(d)
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  writeLines('x', file.path(d, 'index.Rmd'))
  writeLines('y', file.path(d, 'about.md'))
  writeLines('z', file.path(d, '_draft.Rmd'))    # underscore -> excluded
  writeLines('r', file.path(d, 'README.md'))      # readme -> excluded
  b = basename(find_input(d))
  ('index.Rmd' %in% b)
  ('about.md' %in% b)
  (!('_draft.Rmd' %in% b))
  (!('README.md' %in% b))
  # index comes first
  (b[1] %==% 'index.Rmd')
})

assert('find_input() drops a .md file shadowed by a same-stem .Rmd', {
  d = tempfile(); dir.create(d)
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  writeLines('a', file.path(d, 'foo.Rmd'))
  writeLines('b', file.path(d, 'foo.md'))   # shadowed by foo.Rmd -> excluded
  writeLines('c', file.path(d, 'bar.md'))   # kept (no bar.Rmd)
  b = basename(find_input(d))
  ('foo.Rmd' %in% b)
  (!('foo.md' %in% b))
  ('bar.md' %in% b)
})

assert('filter_outdated() flags inputs whose output is missing or stale', {
  f1 = tempfile(); f2 = tempfile()
  writeLines('a', f1); Sys.sleep(.05); writeLines('b', f2)
  # output (f2) is newer than input (f1) -> not outdated
  (isFALSE(filter_outdated(f1, f2, 0)))
  # output newer than input (f1) but input rewritten later -> outdated
  Sys.sleep(.05); writeLines('a2', f1)
  (isTRUE(filter_outdated(f1, f2, 0)))
  # a missing output is always outdated
  (isTRUE(filter_outdated(f1, tempfile(), 0)))
  unlink(c(f1, f2))
})

assert('filter_outdated() flags all outputs when a dependency is newer', {
  f1 = tempfile(); f2 = tempfile(); f3 = tempfile()
  writeLines('a', f1); Sys.sleep(.05); writeLines('b', f2); Sys.sleep(.05)
  writeLines('c', f3)
  (isFALSE(filter_outdated(f1, f2, 0)))
  (isTRUE(filter_outdated(f1, f2, 0, f3)))
  unlink(c(f1, f2, f3))
})

assert('yml_config() reads and normalizes _litedown.yml', {
  d = tempfile(); dir.create(d)
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  # no config -> NULL
  (is.null(yml_config(d)))
  writeLines(c('output:', '  html_document:', '    toc: true'),
             file.path(d, '_litedown.yml'))
  y = yml_config(d)
  # html_document is normalized to the litedown 'html' format
  (names(y$output) %==% 'html')
})

# end-to-end: build the bundled site/book skeletons

assert('fuse_site() renders every input file in a site skeleton to HTML', {
  d = tempfile()
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  proj_skeleton(d, 'site')
  out = fuse_site(d)
  (all(file_exists(out)))
  (file_exists(file.path(d, 'index.html')))
  # the rendered index is non-empty HTML
  (length(readLines(file.path(d, 'index.html'))) > 0L)
})

assert('nav_menu() lists all pages even if only one file is being rendered', {
  d = tempfile()
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  proj_skeleton(d, 'site')
  m = nav_menu(proj_info(file.path(d, 'about.Rmd')))
  (length(m) %==% 2L)
  (grepl('Home', m[1], fixed = TRUE))
  (grepl('/about.html', m[2], fixed = TRUE))
})

assert('nav_menu() ignores doc/index.html (vignette index)', {
  d = tempfile()
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  proj_skeleton(d, 'site')
  for (i in c('doc', 'sub')) {
    dir.create(file.path(d, i))
    writeLines('x', file.path(d, i, 'index.html'))
  }
  m = nav_menu(proj_info(file.path(d, 'about.Rmd')))
  (any(grepl('(/sub/)', m, fixed = TRUE)))
  (!any(grepl('/doc/', m, fixed = TRUE)))
})

assert('fuse_book() renders a book skeleton into a single HTML file', {
  d = tempfile()
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  proj_skeleton(d, 'book')
  out = fuse_book(d)
  (file_exists(out))
  (basename(out) %==% 'index.html')
  h = readLines(out)
  # chapters are wrapped in .chapter divs
  (any(grepl('class="chapter', h, fixed = TRUE)))
})

assert('nav_github() links to the GitHub repo from BugReports in DESCRIPTION', {
  p = tempfile(); dir.create(p)
  on.exit(unlink(p, recursive = TRUE), add = TRUE)
  desc = file.path(p, 'DESCRIPTION')
  writeLines(c('Package: foo', 'BugReports: https://github.com/a/foo/issues'), desc)
  x = nav_github(p)
  (has_match(x, '](https://github.com/a/foo/)', fixed = TRUE, perl = FALSE))
  (has_match(x, 'mark-github-24.svg){.no-invert}', fixed = TRUE, perl = FALSE))
  (has_match(x, '{.right}', fixed = TRUE, perl = FALSE))
  writeLines(c('Package: foo'), desc)
  (is.null(nav_github(p)))
})

assert('set_site_options() marks the site menu with class site-nav', {
  o = options(); on.exit(options(o))
  d = tempfile(); dir.create(d); f = file.path(d, 'a.md'); writeLines('hi', f)
  set_site_options(
    list(meta = list(include_before = 'x', include_after = 'y')), f, d
  )
  (has_match(getOption('litedown.html.meta')$include_before, '^<nav class="site-nav">'))
  (has_match(getOption('litedown.html.meta')$include_after, '^<footer>'))
})
