library(testit)

# page links in a nav menu (excludes icons such as the pages.js toggle and GitHub)
pages = function(m) grep('](/', m, fixed = TRUE, value = TRUE)

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
  x = xfun::read_utf8(file.path(d, 'index.html'))
  (length(x) > 0L)
  (has_match(x, 'nav-highlight[.]min[.]js'))
})

assert('nav_menu() lists all pages even if only one file is being rendered', {
  d = tempfile()
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  proj_skeleton(d, 'site')
  m = nav_menu(proj_info(file.path(d, 'about.Rmd')))
  (length(pages(m)) %==% 2L)
  (has_match(m[1], I('Home')))
  (has_match(m[2], I('/about.html')))
})

assert('nav_menu() labels the home link with the package name and version', {
  d = tempfile()
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  proj_skeleton(d, 'site')
  writeLines(c('Package: foo', 'Version: 1.2.3'), file.path(d, 'DESCRIPTION'))
  m = nav_menu(proj_info(file.path(d, 'about.Rmd')))
  (has_match(m[1], I('[foo](/index.html){.brand}')))
  (has_match(m[1], I('1.2.3')))
  (has_match(m[1], !I('Home')))
})

# write a site config with the given lines under `site`, return the nav menu
nav_with = function(d, ...) {
  writeLines(c('site:', ...), file.path(d, '_litedown.yml'))
  nav_menu(proj_info(file.path(d, 'about.Rmd')))
}

assert('nav_menu() orders items and sets their text from `nav`', {
  d = tempfile()
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  proj_skeleton(d, 'site')
  m = nav_with(d, '  nav:', '    about: About Us', '    index: Start')
  (has_match(m[1], I('[About Us](/about.html)')))
  (has_match(m[2], I('[Start](/index.html)')))
  m = nav_with(d, '  nav: [about, index]')
  (has_match(m[1], I('[About](/about.html)')))
  (has_match(m[2], I('[Home](/index.html)')))
})

assert('nav_menu() puts unlisted items after `nav` items in default order', {
  d = tempfile()
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  proj_skeleton(d, 'site')
  m = nav_with(d, '  nav:', '    about:')
  (has_match(m[1], I('[About](/about.html)')))
  (has_match(m[2], I('[Home](/index.html)')))
})

assert('nav_menu() drops items matching `nav_hide`', {
  d = tempfile()
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  proj_skeleton(d, 'site')
  m = nav_with(d, '  nav_hide: ["ab*"]')
  (length(pages(m)) %==% 1L)
  (has_match(m, I('[Home](/index.html)')))
  m = nav_with(d, '  nav_hide: [about]')
  (has_match(m, !I('/about.html')))
})

assert('nav_menu() errors on unknown or conflicting `nav`/`nav_hide` items', {
  d = tempfile()
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  proj_skeleton(d, 'site')
  (has_error(nav_with(d, '  nav: [nope]')))
  (has_error(nav_with(d, '  nav_hide: [nope]')))
  (has_error(nav_with(d, '  nav: [about]', '  nav_hide: [about]')))
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
  (has_match(m, I('(/sub/)')))
  (has_match(m, !I('/doc/')))
})

assert('fuse_book() renders a book skeleton into a single HTML file', {
  d = tempfile()
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  proj_skeleton(d, 'book')
  out = fuse_book(d)
  (file_exists(out))
  (basename(out) %==% 'index.html')
  h = readLines(out)
  # chapters are wrapped in <article class="chapter">
  (has_match(h, I('<article class="chapter"')))
})

assert('nav_github() links to the GitHub repo from BugReports in DESCRIPTION', {
  p = tempfile(); dir.create(p)
  on.exit(unlink(p, recursive = TRUE), add = TRUE)
  desc = file.path(p, 'DESCRIPTION')
  writeLines(c('Package: foo', 'BugReports: https://github.com/a/foo/issues'), desc)
  x = nav_github(p)
  (has_match(x, I('](https://github.com/a/foo/ "GitHub repository")')))
  (has_match(x, I('mark-github-24.svg){.no-invert}')))
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

assert('set_site_options() points to the manual index when the site has one', {
  o = options(); on.exit(options(o))
  d = tempfile(); dir.create(file.path(d, 'foo'), recursive = TRUE)
  f = file.path(d, 'foo', 'bar.md'); writeLines('hi', f)
  set_site_options(list(meta = list()), f, d)
  (is.null(getOption('litedown.html.meta')$header_includes))
  writeLines('{}', file.path(d, function_index))
  set_site_options(list(meta = list()), f, d)
  m = getOption('litedown.html.meta')
  (has_match(m$header_includes, I('<meta name="function-index" content="../function-index.json">')))
  (m$js2 %==% '@link-functions')
  writeLines('', file.path(d, 'link-functions.js'))
  set_site_options(list(meta = list()), f, d)
  (getOption('litedown.html.meta')$js2 %==% '../link-functions.js')
})

assert('root-relative css/js resolve to the page dir, other paths are kept', {
  o = options(); on.exit(options(o))
  d = tempfile(); dir.create(file.path(d, 'foo'), recursive = TRUE)
  f = file.path(d, 'foo', 'bar.md'); writeLines('hi', f)
  set_site_options(list(meta = list(
    css = '/style.css', js = c('/a.js', '//cdn.test/b.js', 'c.js'),
    css2 = '/extra.css'
  )), f, d)
  m = getOption('litedown.html.meta')
  (m$css %==% '../style.css')
  (m$css2 %==% '../extra.css')
  (m$js %==% c('../a.js', '//cdn.test/b.js', 'c.js'))
})

assert('fuse_site() links root-relative css from pages in subdirs', {
  d = tempfile(); dir.create(file.path(d, 'foo'), recursive = TRUE)
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  writeLines(c('site:', '  rebuild: always', 'css: /style.css'), file.path(d, '_litedown.yml'))
  writeLines('body {}', file.path(d, 'style.css'))
  writeLines('# hi', file.path(d, 'index.md'))
  writeLines('# bar', file.path(d, 'foo', 'bar.md'))
  fuse_site(d)
  x = xfun::read_utf8(file.path(d, 'foo', 'bar.html'))
  (has_match(x, 'href="../style.css"'))
  y = xfun::read_utf8(file.path(d, 'index.html'))
  (has_match(y, 'href="style.css"'))
})

assert('giscus_html() builds a giscus script from site config', {
  (is.null(giscus_html(NULL)))
  x = giscus_html(list(repo = 'a/b', repo_id = 'R_x'))
  (has_match(x, 'data-repo="a/b"'))
  (has_match(x, 'data-repo-id="R_x"'))
  (has_match(x, 'data-strict="1"'))
  (has_match(giscus_html(list(mapping = 'title')), 'data-mapping="title"'))
})

assert('pkg_readme() and pkg_logo() prepare the home page of a package site', {
  d = tempfile(); dir.create(file.path(d, 'man/figures'), recursive = TRUE)
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  p = structure('foo', path = d)
  (is.null(pkg_readme(p)))
  (is.null(pkg_logo(p)))
  file.create(file.path(d, 'man/figures/logo.svg'))
  (pkg_logo(p) %==% '![foo logo](man/figures/logo.svg){.logo}')
  # lines before the badges and the title are dropped; badges go to the sidebar
  writeLines(c(
    '![](man/figures/logo.svg)', '# foo', '<!-- badges: start -->', 'badge',
    '<!-- badges: end -->', 'text'
  ), file.path(d, 'README.md'))
  x = pkg_readme(p)
  (x[1] %==% '::: {.side .side-right}')
  (x[length(x)] %==% 'text')
  (!any(x == '# foo'))
  # a logo in the site dir (current dir) takes precedence
  d2 = tempfile(); dir.create(d2); on.exit(unlink(d2, recursive = TRUE), add = TRUE)
  file.create(file.path(d2, 'logo.png'))
  (in_dir(d2, pkg_logo(p)) %==% '![foo logo](logo.png){.logo}')
  # the logo only in the dropped lines is still added
  (has_match(pkg_logo(p), I('logo.svg')))
  # but not when the README shows it
  writeLines(c('# foo', 'text', '![](man/figures/logo.svg)'), file.path(d, 'README.md'))
  (is.null(pkg_logo(p)))
})

assert('the edit link is in the site nav only if the page has a source file', {
  p = tempfile(); d = file.path(p, 'site')
  dir.create(file.path(p, 'examples'), recursive = TRUE)
  dir.create(file.path(d, 'sub'), recursive = TRUE)
  on.exit(unlink(p, recursive = TRUE), add = TRUE)
  writeLines(c('Package: foo', 'BugReports: https://github.com/u/foo/issues'),
    file.path(p, 'DESCRIPTION'))
  write_utf8('x', file.path(p, 'examples', 'a.Rmd'))
  vars = set_envvar(c(R_LITEDOWN_PKG = p)); on.exit(set_envvar(vars), add = TRUE)
  nav = paste(nav_icons(d), collapse = ' ')
  set_nav = function(input) set_site_options(
    list(meta = list(include_before = nav, include_after = 'y')), input, d
  )
  # the page doesn't exist under the site dir, and its path has a `..` that
  # must be resolved via the existing dir `sub`
  # TODO: drop the version check once xfun 0.62.17 is on CRAN
  set_nav(file.path(d, 'sub', '..', 'examples/a.Rmd'))
  if (packageVersion('xfun') >= '0.62.17')
    (has_match(getOption('litedown.html.meta')$include_before, 'class="edit-page"'))
  set_nav(file.path(d, 'index.Rmd'))
  (has_match(getOption('litedown.html.meta')$include_before, !'edit-page'))
  (has_match(getOption('litedown.html.meta')$include_before, 'pagesjs-toggle'))
  (has_match(nav, I('($edit$ "Edit this page")')))
})
