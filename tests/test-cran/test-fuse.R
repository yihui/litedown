library(testit)

assert('fuse() returns raw_string when output is not a file path', {
  src = c('```{r}', '1 + 1', '```')
  out = fuse(text = src, output = NA)
  # raw_string objects print directly; check that output contains the result
  (grepl('2', out))
})

assert('fuse() evaluates R code chunks and includes output', {
  src = c('```{r}', '1 + 1', '```')
  out = fuse(text = src, output = 'markdown')
  (grepl('2', out))
})

assert('fuse() supports eval = FALSE to skip code execution', {
  src = c('```{r, eval=FALSE}', 'stop("should not run")', '```')
  out = fuse(text = src, output = 'markdown')
  (!grepl('Error', out))
})

assert('fuse() supports echo = FALSE to hide source code', {
  src = c('```{r, echo=FALSE}', '1 + 1', '```')
  out = fuse(text = src, output = 'markdown')
  # output should contain the result but not the code
  (grepl('2', out))
  (!grepl('1 \\+ 1', out))
})

assert('fuse() supports results = FALSE/hide to suppress output', {
  src = c('```{r, results=FALSE}', '1 + 1', '```')
  out = fuse(text = src, output = 'markdown')
  # output should not contain the printed result
  (!grepl('#> 2', out))
})

assert('fuse() supports include = FALSE to suppress all output', {
  src = c('```{r, include=FALSE}', '1 + 1', '```')
  out = fuse(text = src, output = 'markdown')
  (nchar(out) == 0L)
})

assert('fuse() handles error = TRUE to capture errors', {
  src = c('```{r, error=TRUE}', 'stop("oops")', '```')
  out = fuse(text = src, output = 'markdown')
  (grepl('oops', out))
})

assert('fuse() handles warning = TRUE to capture warnings', {
  src = c('```{r, warning=TRUE}', 'warning("careful!")', '```')
  out = fuse(text = src, output = 'markdown')
  (grepl('careful', out))
})

assert('fuse() handles warning = FALSE to suppress warnings', {
  src = c('```{r, warning=FALSE}', 'warning("shh")', '```')
  out = fuse(text = src, output = 'markdown')
  # warning output block should not appear (the source block may still show the code)
  (!grepl('\\.plain \\.warning', out))
})

assert('fuse() handles message = TRUE to capture messages', {
  src = c('```{r, message=TRUE}', 'message("hello from message")', '```')
  out = fuse(text = src, output = 'markdown')
  (grepl('hello from message', out))
})

assert('fuse() handles message = FALSE to suppress messages', {
  src = c('```{r, message=FALSE}', 'message("silent")', '```')
  out = fuse(text = src, output = 'markdown')
  # message output block should not appear
  (!grepl('\\.plain \\.message', out))
})

assert('fuse() evaluates inline code in text blocks', {
  src = 'Value is `{r} 1 + 1`.'
  out = fuse(text = src, output = 'markdown')
  (grepl('Value is 2', out))
})

assert('fuse() handles collapse = TRUE to merge source and output', {
  src = c('```{r, collapse=TRUE}', '1 + 1', '2 + 2', '```')
  out = fuse(text = src, output = 'markdown')
  # source and result should be in the same block
  (grepl('#> \\[1\\] 2', out))
})

assert('fuse() uses comment option to prefix output lines', {
  src = c('```{r, comment="##"}', '1 + 1', '```')
  out = fuse(text = src, output = 'markdown')
  (grepl('##[1] 2', out, fixed = TRUE))
})

assert('fuse() supports comment = "" for no prefix', {
  src = c('```{r, comment=""}', '1 + 1', '```')
  out = fuse(text = src, output = 'markdown')
  (grepl('2', out))
  (!grepl('#>', out))
})

assert('fuse() processes results = "asis" output verbatim', {
  src = c('```{r, results="asis"}', 'cat("<p>asis-output</p>\\n")', '```')
  out = fuse(text = src, output = 'markdown')
  # the cat() output should appear as raw HTML (no code fence around it)
  (grepl('<p>asis-output</p>', out, fixed = TRUE))
})

assert('fuse() handles text input without code chunks', {
  src = '# My Document\n\nJust some text.'
  out = fuse(text = src, output = 'markdown')
  (grepl('My Document', out))
  (grepl('Just some text', out))
})

assert('code blocks after asis HTML output are rendered correctly (regression)', {
  # regression: asis output containing HTML tags must not cause following code
  # blocks to be treated as HTML content (the resulting HTML should have no
  # triple backticks)
  out = fuse(text = '#| results="asis"\ncat("<p>hi</p>\\n")\n#| foo\n1:2')
  (!grepl('```', out))
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
    '```{r}',
    paste0('fuse("', b, '", output = "markdown")'),
    '```',
    '', '```{r}', 'plot(2)', '```'
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
