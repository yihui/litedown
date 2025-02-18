for (f in list.files('../examples', '[.]Rmd$', full.names = TRUE)) {
  m = file.mtime(c(f, f2 <- xfun::with_ext(f, '.md')))
  if (!is.na(m[2]) && m[1] < m[2]) next  # re-render only if output file is newer
  xfun::Rscript_call(function(.) {
    options(width = 80, litedown.jsd_resolve = FALSE)
    litedown::fuse(., '.md')
    if (grepl('/014-.+[.]Rmd$', .)) litedown::fiss(.)
  }, list(f))
}
