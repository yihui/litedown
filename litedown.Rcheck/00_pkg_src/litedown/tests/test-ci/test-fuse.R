library(testit)

assert('fuse() fig.path option controls plot file location', {
  src = c(
    '---', 'output:', '  html:', '    options:', '      embed_resources: false',
    '---', '', '```{r}', 'plot(1)', '```'
  )
  old = litedown::reactor(fig.path = 'foo')
  out = litedown::fuse(text=src, output = 'markdown')
  litedown::reactor(old)
  # the figure reference in markdown should use our custom path prefix
  (any(grepl('foochunk-1-1.png', out, fixed = TRUE)))
  unlink('foochunk-1-1.png')
})

assert('fuse() fig.path with subdirectory creates and uses the directory', {
  src = c(
    '---', 'output:', '  html:', '    options:', '      embed_resources: false',
    '---', '', '```{r}', 'plot(1)', '```'
  )
  old = litedown::reactor(fig.path = 'foo/bar')
  out = litedown::fuse(text=src, output = 'markdown')
  litedown::reactor(old)
  (any(grepl('foo/barchunk-1-1.png', out, fixed = TRUE)))
  unlink('foo', recursive = TRUE)
})
