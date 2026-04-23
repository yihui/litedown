library(testit)

assert('fuse() evaluates inline code in text blocks', {
  src = 'Value is `{r} 1 + 1`.'
  out = fuse(text = src, output = 'markdown')
  ('Value is 2.' %==% as.character(out))
})

assert('fuse() handles text input without code chunks', {
  src = '# My Document\n\nJust some text.'
  out = fuse(text = src, output = 'markdown')
  (src %==% as.character(out))
})

assert('code blocks after asis HTML output are rendered correctly (regression)', {
  # regression: asis output containing HTML tags must not cause following code
  # blocks to be treated as HTML content (the resulting HTML should have no
  # triple backticks)
  out = fuse(text = '#| results="asis"\ncat("<p>hi</p>\\n")\n#| foo\n1:2')
  (!grepl('```', out))
  (as.character(gsub('.*<pre><code class="language-r">1:2.*', '', out)) %==% '')
})

assert('fuse() fig.path option controls plot file location', {
  src = c(
    '---', 'output:', '  html:', '    options:', '      embed_resources: false',
    '---', '', '```{r}', 'plot(1)', '```'
  )
  old = reactor(fig.path = 'foo')
  out = fuse(text = src, output = 'markdown')
  reactor(old)
  (any(grepl('foochunk-1-1.png', out, fixed = TRUE)))
  (file_exists('foochunk-1-1.png'))
  unlink('foochunk-1-1.png')

  old = reactor(fig.path = 'foo/bar')
  out = fuse(text = src, output = 'markdown')
  reactor(old)
  (any(grepl('foo/barchunk-1-1.png', out, fixed = TRUE)))
  (file_exists(file.path('foo', 'barchunk-1-1.png')))
  unlink('foo', recursive = TRUE)
})

assert('fuse() does not let nested fuse() override outer plot files (#127)', {
  b = tempfile(fileext = '.Rmd')
  a = tempfile(fileext = '.Rmd')
  writeLines(c('```{r}', 'plot(1)', '```'), b)
  writeLines(c(
    '```{r}', 'plot(1)', '```', '',
    '```{r}',
    paste0('fuse(b, output = "markdown")'),
    '```'
  ), a)
  fuse(a, output = 'markdown')
  a_files = paste0(tools::file_path_sans_ext(a), '__files')
  b_files = paste0(tools::file_path_sans_ext(b), '__files')
  # outer and inner plots must be in separate directories
  (dir.exists(a_files))
  (dir.exists(b_files))
  (length(list.files(a_files, '\\.png$')) > 0L)
  (length(list.files(b_files, '\\.png$')) > 0L)
  unlink(c(a, b, a_files, b_files), recursive = TRUE)
})


assert('fiss() extracts R code from an R Markdown document', {
  src = c('```{r}', 'x = 1', '```', 'text', '```{r}', 'x + 1', '```')
  out = fiss(I(src))
  (c('x = 1', '', 'x + 1', '') %==% as.character(out))
})

assert('fiss() respects purl = FALSE chunk option', {
  src = c('```{r, purl=FALSE}', 'secret = 1', '```', '```{r}', 'public = 2', '```')
  out = fiss(I(src))
  (c('public = 2', '') %==% as.character(out))
})
