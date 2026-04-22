library(testit)

assert('fuse() returns raw_string when output is not a file path', {
  src = c('```{r}', '1 + 1', '```')
  out = litedown::fuse(text=src, output = NA)
  (inherits(out, 'xfun_raw_string'))
})

assert('fuse() evaluates R code chunks and includes output', {
  src = c('```{r}', '1 + 1', '```')
  out = litedown::fuse(text=src, output = 'markdown')
  (grepl('2', out))
})

assert('fuse() supports eval = FALSE to skip code execution', {
  src = c('```{r, eval=FALSE}', 'stop("should not run")', '```')
  out = litedown::fuse(text=src, output = 'markdown')
  (is.character(out))
  (!grepl('Error', out))
})

assert('fuse() supports echo = FALSE to hide source code', {
  src = c('```{r, echo=FALSE}', '1 + 1', '```')
  out = litedown::fuse(text=src, output = 'markdown')
  # output should contain the result but not the code
  (grepl('2', out))
  (!grepl('1 \\+ 1', out))
})

assert('fuse() supports results = FALSE/hide to suppress output', {
  src = c('```{r, results=FALSE}', '1 + 1', '```')
  out = litedown::fuse(text=src, output = 'markdown')
  # output should not contain the printed result
  (!grepl('#> 2', out))
})

assert('fuse() supports include = FALSE to suppress all output', {
  src = c('```{r, include=FALSE}', '1 + 1', '```')
  out = litedown::fuse(text=src, output = 'markdown')
  (nchar(out) == 0L)
})

assert('fuse() handles error = TRUE to capture errors', {
  src = c('```{r, error=TRUE}', 'stop("oops")', '```')
  out = litedown::fuse(text=src, output = 'markdown')
  (grepl('oops', out))
})

assert('fuse() handles warning = TRUE to capture warnings', {
  src = c('```{r, warning=TRUE}', 'warning("careful!")', '```')
  out = litedown::fuse(text=src, output = 'markdown')
  (grepl('careful', out))
})

assert('fuse() handles warning = FALSE to suppress warnings', {
  src = c('```{r, warning=FALSE}', 'warning("shh")', '```')
  out = litedown::fuse(text=src, output = 'markdown')
  # warning output block should not appear (the source block may still show the code)
  (!grepl('\\.plain \\.warning', out))
})

assert('fuse() handles message = TRUE to capture messages', {
  src = c('```{r, message=TRUE}', 'message("hello from message")', '```')
  out = litedown::fuse(text=src, output = 'markdown')
  (grepl('hello from message', out))
})

assert('fuse() handles message = FALSE to suppress messages', {
  src = c('```{r, message=FALSE}', 'message("silent")', '```')
  out = litedown::fuse(text=src, output = 'markdown')
  # message output block should not appear
  (!grepl('\\.plain \\.message', out))
})

assert('fuse() evaluates inline code in text blocks', {
  src = 'Value is `{r} 1 + 1`.'
  out = litedown::fuse(text=src, output = 'markdown')
  (grepl('Value is 2', out))
})

assert('fuse() handles collapse = TRUE to merge source and output', {
  src = c('```{r, collapse=TRUE}', '1 + 1', '2 + 2', '```')
  out = litedown::fuse(text=src, output = 'markdown')
  (is.character(out))
})

assert('fuse() uses comment option to prefix output lines', {
  src = c('```{r, comment="##"}', '1 + 1', '```')
  out = litedown::fuse(text=src, output = 'markdown')
  (grepl('##[1] 2', out, fixed = TRUE))
})

assert('fuse() supports comment = "" for no prefix', {
  src = c('```{r, comment=""}', '1 + 1', '```')
  out = litedown::fuse(text=src, output = 'markdown')
  (grepl('2', out))
  (!grepl('#>', out))
})

assert('fuse() processes results = "asis" output verbatim', {
  src = c('```{r, results="asis"}', 'cat("<p>asis-output</p>\\n")', '```')
  out = litedown::fuse(text=src, output = 'markdown')
  # the cat() output should appear as raw HTML (no code fence around it)
  (grepl('<p>asis-output</p>', out, fixed = TRUE))
})

assert('fuse() handles text input without code chunks', {
  src = '# My Document\n\nJust some text.'
  out = litedown::fuse(text=src, output = 'markdown')
  (grepl('My Document', out))
  (grepl('Just some text', out))
})

assert('code blocks after asis HTML output are rendered correctly (regression)', {
  # regression: asis output containing HTML tags must not cause following code
  # blocks to be treated as HTML content (the resulting HTML should have no
  # triple backticks)
  out = litedown::fuse(text = '#| results="asis"\ncat("<p>hi</p>\\n")\n#| foo\n1:2')
  (!grepl('```', out))
})

assert('fuse() resets fig.path and cache.path in nested calls (#127)', {
  # nested fuse() should not inherit outer fig.path/cache.path
  outer_src = c(
    '```{r}',
    'litedown::fuse(text=c("```{r}", "1+1", "```"), output = "markdown")',
    '```'
  )
  old = litedown::reactor(fig.path = 'outer__files/')
  out = litedown::fuse(text=outer_src, output = 'markdown')
  litedown::reactor(old)
  # the nested fuse() should succeed and return a character result
  (is.character(out))
})
