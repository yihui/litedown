library(testit)

# use the installed litedown package itself as a stable test fixture
pkg = 'litedown'

# test_pkg() installs a minimal build with no help database, so Rd-based
# functions (Rd_aliases(), pkg_manual()) can't run there; gate those tests.
has_rd = !inherits(try(tools::Rd_db(pkg), silent = TRUE), 'try-error')

# NEWS.md is not always installed (e.g., older R versions don't ship it in the
# package's installed dir), so gate NEWS-based tests on its presence.
has_news = file_exists(detect_news(pkg))

assert('header_class() builds the {.unlisted .unnumbered} attribute string', {
  # both off -> both classes (Markdown form)
  (header_class(FALSE, FALSE) %==% ' {.unlisted .unnumbered}')
  # toc on but numbering off -> only .unnumbered
  (header_class(TRUE, FALSE) %==% ' {.unnumbered}')
  # both on -> empty
  (header_class(TRUE, TRUE) %==% '')
  # HTML form uses class="..."
  (header_class(FALSE, FALSE, FALSE) %==% ' class="unlisted unnumbered"')
  (header_class(FALSE, TRUE, FALSE) %==% ' class="unlisted"')
})

assert('github_link() extracts the repo URL from DESCRIPTION BugReports', {
  u = github_link(system.file(package = pkg))
  (grepl('^https://github.com/[^/]+/[^/]+/$', u))
  # a directory with no DESCRIPTION errors (read.dcf fails)
  (has_error(suppressWarnings(github_link(tempdir()))))
})

assert('github_file_url() links existing files to GitHub', {
  p = tempfile(); dir.create(p)
  on.exit(unlink(p, recursive = TRUE), add = TRUE)
  write_utf8('a', file.path(p, 'a.Rmd'))
  write_utf8('a', file.path(p, 'a.md'))
  (is.null(github_file_url(p, 'a.Rmd')))  # no DESCRIPTION
  writeLines(c('Package: foo', 'BugReports: https://github.com/u/foo/issues'),
    file.path(p, 'DESCRIPTION'))
  (github_file_url(p, 'a.Rmd', TRUE) %==% 'https://github.com/u/foo/edit/HEAD/a.Rmd')
  (github_file_url(p, 'a.Rmd') %==% 'https://github.com/u/foo/blob/HEAD/a.Rmd')
  (github_file_url(p, 'a.md') %==% 'https://github.com/u/foo/blob/HEAD/a.md?plain=1')
  # the cache is invalidated when DESCRIPTION is modified
  Sys.sleep(1.1)
  writeLines(c('Package: foo', 'BugReports: https://github.com/v/foo/issues'),
    file.path(p, 'DESCRIPTION'))
  (github_file_url(p, 'a.Rmd') %==% 'https://github.com/v/foo/blob/HEAD/a.Rmd')
  (is.null(github_file_url(p, 'b.Rmd')))  # file does not exist
})

assert('pkg_examples() links sources to GitHub when possible', {
  p = tempfile(); d = file.path(p, 'examples')
  dir.create(d, recursive = TRUE)
  on.exit(unlink(p, recursive = TRUE), add = TRUE)
  write_utf8('x', file.path(d, '001-hi.Rmd'))
  writeLines(c('Package: foo', 'BugReports: https://github.com/u/foo/issues'),
    file.path(p, 'DESCRIPTION'))
  x = in_dir(tempdir(), pkg_examples(p))
  (has_match(x, '(https://github.com/u/foo/blob/HEAD/examples/001-hi.Rmd)'))
})

assert('edit_page_url() finds the source of a site page in the package', {
  p = tempfile(); d = file.path(p, 'site')
  dir.create(file.path(p, 'examples'), recursive = TRUE); dir.create(d)
  on.exit(unlink(p, recursive = TRUE), add = TRUE)
  writeLines(c('Package: foo', 'BugReports: https://github.com/u/foo/issues'),
    file.path(p, 'DESCRIPTION'))
  write_utf8('x', file.path(p, 'examples', 'a.Rmd'))
  vars = set_envvar(c(R_LITEDOWN_PKG = p)); on.exit(set_envvar(vars), add = TRUE)
  (edit_page_url(file.path(d, 'examples/a.Rmd'), d) %==%
    'https://github.com/u/foo/edit/HEAD/examples/a.Rmd')
  (is.null(edit_page_url(file.path(d, 'index.Rmd'), d)))
})

assert('pkg_authors() falls back to the Author field with no Authors@R', {
  (pkg_authors(list(Author = 'Jane Doe')) %==% 'Jane Doe')
})

assert('pkg_authors() formats Authors@R with roles, URLs, and ORCID links', {
  orcid = '0000-0003-0645-5666'
  desc = list('Authors@R' = sprintf(paste0(
    'person("Jane", "Doe", role = c("aut", "cre"), ',
    'comment = c(ORCID = "%s", URL = "https://example.org"))'
  ), orcid))
  a = pkg_authors(desc)
  (grepl('Jane Doe', a))
  (grepl('https://example.org', a))       # name linked to URL
  (grepl(paste0('orcid.org/', orcid), a))  # ORCID badge
  (grepl('\\[aut, cre\\]', a))             # roles shown
  # role filtering: request only 'cre' keeps the person; 'ctb' drops them
  (length(pkg_authors(desc, role = 'cre')) == 1L)
  (length(pkg_authors(desc, role = 'ctb')) == 0L)
  # extra = FALSE drops roles/ORCID
  (!grepl('orcid', pkg_authors(desc, extra = FALSE)))
})

assert('tweak_citation() fills in a missing year', {
  x = tweak_citation(suppressWarnings(citation(pkg)))
  (!is.null(unclass(x)[[1]]$year))
})

if (has_news) assert('detect_news() finds NEWS.md via the package path or installed package', {
  # installed package: falls back to system.file()
  p = detect_news(pkg)
  (is.character(p))
  (basename(p) %==% 'NEWS.md')
})

if (has_rd) assert('Rd_aliases() extracts \\alias entries from an Rd object', {
  db = tools::Rd_db(pkg)
  nm = names(db)[1]
  al = Rd_aliases(db[[nm]])
  (is.character(al))
  (length(al) >= 1L)
})

assert('pkg_desc() renders package metadata as an HTML table or definition list', {
  tab = pkg_desc(pkg, 'table')
  (any(grepl('<table', tab)))
  (any(grepl('Version', tab)))
  # single quotes in Title/Description are stripped (sans_sq)
  dl = pkg_desc(pkg, 'dl')
  (any(grepl('<dl', dl)))
  (any(grepl('<dt>Version</dt>', dl, fixed = TRUE)))
})

assert('pkg_desc(type = "dl") generates a compact sidebar', {
  dl = pkg_desc(pkg, 'dl')
  (!any(grepl('<dt>Description</dt>', dl, fixed = TRUE)))
  (!any(grepl('<dt>(URL|BugReports|Suggests)</dt>', dl)))
  (any(grepl('<dt>Links</dt>', dl, fixed = TRUE)))
  (any(grepl('>Report a bug</a>', dl, fixed = TRUE)))
  # wrapped in a sidebar Div
  (dl[1] %==% '::: {.side .side-right}')
  (':::' %in% dl)
})

assert('side_links() labels URLs by host and appends the bug tracker', {
  (side_links(NA, NA) %==% NULL)
  x = side_links(
    'https://github.com/a/b, https://example.org/pkg/', 'https://github.com/a/b/issues'
  )
  (side_links('https://dev.azure.com/o/p/_git/r', NA) %==%
    '[Browse source code](https://dev.azure.com/o/p/_git/r)')
  (x %==% paste0(
    '[Browse source code](https://github.com/a/b)\\\n',
    '[example.org](https://example.org/pkg/)\\\n',
    '[Report a bug](https://github.com/a/b/issues)'
  ))
})

assert('pkg_citation() returns text and BibTeX citations', {
  ci = suppressWarnings(pkg_citation(pkg))
  (length(ci) > 0L)
  # a fenced ``` latex block holds the BibTeX entry
  (any(grepl('``` latex', ci, fixed = TRUE)))
  (any(grepl('@Manual|@Misc|@Article', ci)))
})

if (has_news) assert('pkg_news() returns news entries with lowered heading levels', {
  n = suppressWarnings(pkg_news(pkg, recent = 1))
  (length(n) > 0L)
  # NEWS.md top-level '# ' headings are lowered to '## '
  (any(grepl('^## ', n)))
})

assert('pkg_news() links issues and users right after a parenthesis', {
  d = tempfile(); dir.create(d)
  on.exit(unlink(d, recursive = TRUE))
  writeLines(
    'BugReports: https://github.com/a/b/issues', file.path(d, 'DESCRIPTION')
  )
  writeLines(
    c('# CHANGES IN x VERSION 1.0', '', '- Fix (#1, #2) by (@u).'),
    file.path(d, 'NEWS.md')
  )
  n = unclass(pkg_news('x', file.path(d, 'NEWS.md')))[3]
  (n %==% paste0(
    '- Fix ([#1](https://github.com/a/b/issues/1), ',
    '[#2](https://github.com/a/b/issues/2)) by ([@u](https://github.com/u)).'
  ))
})

if (has_rd) assert('pkg_manual() renders all man pages with section ids and a TOC', {
  m = pkg_manual(pkg, toc = TRUE, number_sections = FALSE, examples = FALSE)
  (length(m) > 0L)
  # each man page heading carries an id of the form sec:man-<topic>
  (any(grepl('id="sec:man-', m)))
  # the alias TOC links back to those sections
  (any(grepl('href="#sec:man-', m)))
  # the default index is also in a callout
  (any(grepl('{.callout-tip data-legend="Index"}', m, fixed = TRUE)))
})

assert('group_items() assigns items to groups with wildcards and an Other group', {
  keys = list(c('a_x', 'a'), c('q', 'b'), 'b_z', 'misc')
  groups = list(
    list(title = 'A', desc = 'd', contents = 'a_*'),
    list(title = 'Z', contents = 'nothing'),
    list(title = 'B', contents = c('b_z', 'b'))
  )
  # contents that match no item are an error
  (has_error(group_items(keys, groups), 'nothing'))
  groups[[2]] = NULL
  g = group_items(keys, groups)
  (sapply(g, `[[`, 'title') %==% c('A', 'B', 'Other'))
  (g[[1]]$i %==% 1L)
  (g[[2]]$i %==% c(2L, 3L))  # matches B via the second key
  (g[[3]]$i %==% 4L)
  # skipped items are included only when matched by a group
  g = group_items(keys, groups, skip = c(FALSE, FALSE, TRUE, TRUE))
  (g[[2]]$i %==% c(2L, 3L))
  (length(g) %==% 2L)  # item 4 is skipped, so no Other group
})

if (has_rd) assert('pkg_manual() groups help pages when groups are given', {
  m = pkg_manual(
    pkg, examples = FALSE,
    groups = list(list(title = 'First', desc = 'My desc', contents = 'pkg_*'))
  )
  (has_match(m, '<h3 class="unnumbered unlisted">First</h3>'))
  (has_match(m, '<h3 class="unnumbered unlisted">Other</h3>'))
  (!any(grepl('<kbd>', m)))
  (has_match(m, '<p>My desc</p>'))
  (has_match(m, '::: {.callout-tip data-legend="Index"}', fixed = TRUE, perl = FALSE))
  (has_match(m, 'callout[.](min[.])?css'))
  # the help pages must still follow the index
  (any(grepl('<h2 id="sec:man-', m)))
})

vp = Find(function(p) nzchar(system.file(package = p)), c('knitr', 'rmarkdown'))
if (!is.null(vp) && nrow(tools::getVignetteInfo(vp)))
  assert('pkg_articles() lists vignettes, optionally in groups', {
    d = tempfile(); dir.create(d)
    on.exit(unlink(d, recursive = TRUE), add = TRUE)
    x = in_dir(d, pkg_articles(vp))
    (any(grepl('^- \\[', x)))
    x = in_dir(d, pkg_articles(vp, groups = list(list(title = 'G', desc = 'dd', contents = '*'))))
    (x[1] %==% '## G\n')
    (x[2] %==% 'dd')
    (!any(grepl('## Other', x)))
  })

assert('pkg_articles() reports packages without vignettes', {
  (pkg_articles('base') %==% 'This package has no vignettes.')
})

assert('site_groups() reads groups from the site field of _litedown.yml', {
  d = tempfile(); dir.create(d)
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  writeLines(c(
    'site:', '  manual:', '    A: [x, "y*"]', '    B:', '      desc: dd',
    '      contents: [z]'
  ), file.path(d, '_litedown.yml'))
  g = in_dir(d, site_groups('manual'))
  (g[[1]] %==% list(title = 'A', contents = c('x', 'y*')))
  (g[[2]] %==% list(title = 'B', desc = 'dd', contents = 'z'))
  (is.null(in_dir(d, site_groups('articles'))))
  (is.null(in_dir(tempdir(), site_groups('manual'))))
})

assert('side_links() drops the URL of the package site', {
  u = 'https://a.github.io/pkg/, https://github.com/a/pkg'
  x = side_links(u, 'https://github.com/a/pkg/issues', 'pkg')
  (!grepl('github.io', x))
  (grepl('Browse source code', x))
  # the site URL can be set in _litedown.yml
  d = tempfile(); dir.create(d)
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  writeLines(c('site:', '  url: https://pkg.example.org/'), file.path(d, '_litedown.yml'))
  x = in_dir(d, side_links('https://pkg.example.org, https://github.com/a/pkg', NA, 'pkg'))
  (!grepl('example.org', x))
  (grepl('Browse source code', x))
})

assert('site_cleanup() removes the template sources but not assets or your files', {
  d = tempfile(); p = tempfile()
  dir_create(file.path(p, 'vignettes'))
  writeLines('Package: foo', file.path(p, 'DESCRIPTION'))
  on.exit(unlink(c(d, p), recursive = TRUE), add = TRUE)
  dir_create(d)
  write_utf8('custom', file.path(d, 'index.Rmd'))
  copied = site_skeleton(d, p)
  (!('index.Rmd' %in% copied))  # it was not copied from the template
  (all(c('manual.Rmd', '_litedown.yml', 'playground/index.html') %in% copied))
  site_cleanup(d, copied)
  fs = list.files(d, recursive = TRUE)
  (!any(c('manual.Rmd', '_litedown.yml', '_footer.Rmd') %in% fs))
  ('index.Rmd' %in% fs)  # your own file stays
  ('playground/index.html' %in% fs)  # assets stay
})

assert('site_exclude() reads the pages to exclude from _litedown.yml', {
  d = tempfile(); p = tempfile()
  dir_create(file.path(p, 'vignettes'))
  writeLines('Package: foo', file.path(p, 'DESCRIPTION'))
  on.exit(unlink(c(d, p), recursive = TRUE), add = TRUE)
  dir_create(d)
  (site_exclude(d) %==% 'code.Rmd')  # default
  cfg = file.path(d, '_litedown.yml')
  writeLines(c('site:', '  exclude: [code.Rmd, index.Rmd]'), cfg)
  (site_exclude(d) %==% c('code.Rmd', 'index.Rmd'))
  site_skeleton(d, p)
  (!any(c('code.Rmd', 'index.Rmd') %in% list.files(d)))
  (file_exists(file.path(d, 'manual.Rmd')))
  # NA means to exclude nothing
  writeLines(c('site:', '  exclude: NA'), cfg)
  (length(site_exclude(d)) %==% 0L)
  site_skeleton(d, p)
  (file_exists(file.path(d, 'code.Rmd')))
})

assert('pkg_examples() lists examples from the package root, not the working dir', {
  p = tempfile(); d = file.path(p, 'examples')
  dir.create(d, recursive = TRUE)
  on.exit(unlink(p, recursive = TRUE), add = TRUE)
  write_utf8(c('---', 'title: Hello', '---'), file.path(d, '001-hi.Rmd'))
  x = in_dir(tempdir(), pkg_examples(p))
  (length(x) > 0L)
  (any(grepl('001\\. [Hello](', x, fixed = TRUE)))
  (any(grepl('.box', x, fixed = TRUE)))
  (any(grepl('001-hi.html)', x, fixed = TRUE)))
  # the boxes are in a container
  (any(grepl('{.boxes}', x, fixed = TRUE)))
  x = pkg_examples(p, 'nope')
  (grepl("'nope/'", x, fixed = TRUE))
})

assert('pkg_examples() puts examples in groups', {
  p = tempfile(); d = file.path(p, 'examples')
  dir.create(d, recursive = TRUE)
  on.exit(unlink(p, recursive = TRUE), add = TRUE)
  write_utf8(c('---', 'title: Hello', '---'), file.path(d, '001-hi.Rmd'))
  write_utf8(c('---', 'title: Plots', '---'), file.path(d, '002-plot.Rmd'))
  write_utf8(c('---', 'title: Misc', '---'), file.path(d, '003-misc.Rmd'))
  groups = list(
    list(title = 'First', desc = 'My desc', contents = '001*'),
    list(title = 'Graphics', contents = 'Plots')  # matches the title
  )
  x = in_dir(tempdir(), pkg_examples(p, groups = groups))
  i = grep('^## ', x)
  (x[i] %==% c('## First\n', '## Graphics\n', '## Other\n'))
  (any(x == 'My desc'))
  # each group has its own container
  (sum(grepl('{.boxes}', x, fixed = TRUE)) %==% 3L)
  # each example is in its group, in the order of the groups
  (grep('001-hi.html)', x, fixed = TRUE) < i[2])
  (all(grep('002-plot.html)', x, fixed = TRUE) %in% (i[2]:(i[3] - 1))))
  (all(grep('003-misc.html)', x, fixed = TRUE) > i[3]))
  (has_error(pkg_examples(p, groups = list(list(title = 'X', contents = 'nope*')))))
})

assert('pkg_site() requires the package to be installed', {
  (has_error(pkg_site('an-uninstalled-package')))
})

# the site template is under inst/, which may not be installed for tests
if (system.file('site', package = pkg) != '')
  assert('site_skeleton() copies the site template and deletes pages that do not apply', {
    d = tempfile(); p = tempfile()
    dir_create(file.path(p, 'vignettes'))
    writeLines('Package: foo', file.path(p, 'DESCRIPTION'))
    site_skeleton(d, p)
    fs = list.files(d)
    ('index.Rmd' %in% fs)
    # vignettes/ exists, so articles.Rmd is kept
    ('articles.Rmd' %in% fs)
    # no NEWS.md/inst/NEWS.Rd and no examples/ -> the pages are deleted
    (!('news.Rmd' %in% fs))
    (!('examples.Rmd' %in% fs))
    # files that already exist in the site dir are not overwritten
    write_utf8('custom', f <- file.path(d, 'index.Rmd'))
    site_skeleton(d, p)
    (read_utf8(f) %==% 'custom')
    (!file_exists(file.path(d, 'code.Rmd')))
    unlink(c(d, p), recursive = TRUE)
  })
