local({
  if (!file.exists(f <- '../inst/examples/render-options.R'))
    f = litedown:::pkg_file('examples', 'render-options.R')
  source(f, local = TRUE, echo = TRUE)
})
