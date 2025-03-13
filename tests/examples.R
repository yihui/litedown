# rebuild all examples on CI and check if their output changes
ci = tolower(Sys.getenv('CI')) == 'true'

for (f in litedown:::find_input('../examples')) {
  # re-render locally only if output file is newer
  if (!ci) {
    m = file.mtime(c(f, f2 <- xfun::with_ext(f, '.md')))
    if (!is.na(m[2]) && m[1] < m[2]) next
  }
  xfun::Rscript_call(function(.) {
    options(width = 80, litedown.jsd_resolve = FALSE)
    if (xfun::file_ext(.) == 'md') litedown::mark(.) else litedown::fuse(., '.md')
    if (grepl('/014-.+[.]Rmd$', .)) litedown::fiss(.)
  }, list(f))
}

if (ci && Sys.which('git') != '' && length(system2('git', 'diff', stdout = TRUE))) {
  system2('git', c('diff', '--color'))
  stop('Changes detected in examples output.')
}
