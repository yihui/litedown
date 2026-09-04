library(testit)

# use the installed litedown package itself as a stable test fixture
pkg = 'litedown'

# test_pkg() installs a minimal build with no help database, so Rd-based
# functions (Rd_aliases(), pkg_manual()) can't run there; gate those tests.
has_rd = !inherits(try(tools::Rd_db(pkg), silent = TRUE), 'try-error')

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

assert('detect_news() finds NEWS.md via the package path or installed package', {
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

assert('pkg_citation() returns text and BibTeX citations', {
  ci = suppressWarnings(pkg_citation(pkg))
  (length(ci) > 0L)
  # a fenced ``` latex block holds the BibTeX entry
  (any(grepl('``` latex', ci, fixed = TRUE)))
  (any(grepl('@Manual|@Misc|@Article', ci)))
})

assert('pkg_news() returns news entries with lowered heading levels', {
  n = suppressWarnings(pkg_news(pkg, recent = 1))
  (length(n) > 0L)
  # NEWS.md top-level '# ' headings are lowered to '## '
  (any(grepl('^## ', n)))
})

if (has_rd) assert('pkg_manual() renders all man pages with section ids and a TOC', {
  m = pkg_manual(pkg, toc = TRUE, number_sections = FALSE, examples = FALSE)
  (length(m) > 0L)
  # each man page heading carries an id of the form sec:man-<topic>
  (any(grepl('id="sec:man-', m)))
  # the alias TOC links back to those sections
  (any(grepl('href="#sec:man-', m)))
})
