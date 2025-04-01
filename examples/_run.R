# rebuild all examples on CI and check if their output changes
ci = tolower(Sys.getenv('CI')) == 'true'

for (f in litedown:::find_input('.')) {
  # re-render locally only if output file is newer
  if (!ci) {
    m = file.mtime(c(f, f2 <- xfun::with_ext(f, '.md')))
    if (!is.na(m[2]) && m[1] < m[2]) next
  }
  # example 005 uses |>, which is available only in R >= 4.1.0
  if (grepl('^005-', f) && getRversion() < '4.1.0') next
  xfun::Rscript_call(function(.) {
    options(width = 80, litedown.jsd_resolve = FALSE)
    if (xfun::file_ext(.) == 'md') litedown::mark(.) else litedown::fuse(., '.md')
    if (grepl('^014-.+[.]Rmd$', .)) litedown::fiss(.)
  }, list(f))
}
