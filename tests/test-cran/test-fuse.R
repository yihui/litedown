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
  out = fuse(a, output = 'markdown')
  a_files = paste0(tools::file_path_sans_ext(a), '__files')
  b_files = paste0(tools::file_path_sans_ext(b), '__files')
  (any(grepl(basename(a_files), out, fixed = TRUE)))
  (any(grepl(basename(b_files), out, fixed = TRUE)))
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

# fuse() code chunk output ---------------------------------------------------

md = function(...) as.character(fuse(..., output = 'markdown'))

assert('fuse() echoes source and prints the value of a code chunk', {
  out = md(text = c('```{r}', '1 + 1', '```'))
  (grepl('1 + 1', out, fixed = TRUE))          # source echoed
  (grepl('#> [1] 2', out, fixed = TRUE))  # printed value with default comment
})

assert('fuse() honors echo=FALSE and eval=FALSE', {
  # echo=FALSE hides the source but keeps the output
  out = md(text = c('```{r echo=FALSE}', '1 + 1', '```'))
  (!grepl('1 + 1', out, fixed = TRUE))
  (grepl('#> [1] 2', out, fixed = TRUE))
  # eval=FALSE keeps the source but produces no output
  out = md(text = c('```{r eval=FALSE}', '1 + 1', '```'))
  (grepl('1 + 1', out, fixed = TRUE))
  (!grepl('#>', out, fixed = TRUE))
})

assert('fuse() captures errors, warnings, and messages with error=TRUE', {
  # error=TRUE turns a stop() into an error block instead of aborting
  out = md(text = c('```{r error=TRUE}', 'stop("boom")', '```'))
  (grepl('.error', out, fixed = TRUE))
  (grepl('boom', out))
  # warnings and messages get their own labeled blocks
  (grepl('.warning', md(text = c('```{r}', 'warning("careful")', '```')), fixed = TRUE))
  (grepl('.message', md(text = c('```{r}', 'message("hi")', '```')), fixed = TRUE))
})

assert('fuse() supports results="hide" and results="asis"', {
  # hide: source kept, printed value suppressed
  out = md(text = c('```{r results="hide"}', '1 + 1', '```'))
  (!grepl('#>', out, fixed = TRUE))
  # asis: cat() output is emitted as raw Markdown (a real heading)
  out = md(text = c('```{r results="asis"}', 'cat("# Heading")', '```'))
  (grepl('# Heading', out, fixed = TRUE))
})

assert('fuse() collapses source and output when collapse=TRUE', {
  out = md(text = c('```{r collapse=TRUE}', '1 + 1', '2 + 2', '```'))
  # a single fenced block holds both the code and the interleaved output
  (grepl('1 + 1\n#> [1] 2', out, fixed = TRUE))
  (grepl('2 + 2\n#> [1] 4', out, fixed = TRUE))
})

assert('fuse() uses a custom output comment prefix', {
  out = md(text = c('```{r comment="##"}', '1 + 1', '```'))
  (grepl('##[1] 2', out, fixed = TRUE))
})

assert('fuse() evaluates inline code and formats numbers as LaTeX math', {
  # a large number switches to scientific notation rendered as math
  (grepl('$10^{6}$', md(text = 'Val: `{r} 1e6`.'), fixed = TRUE))
  # a small integer is printed verbatim
  (grepl('Val: 42.', md(text = 'Val: `{r} 42`.'), fixed = TRUE))
  # a character result is inserted as-is
  (grepl('Name: ab.', md(text = 'Name: `{r} paste0("a", "b")`.'), fixed = TRUE))
})

assert('fuse() exposes fuse_env() and get_context() during evaluation', {
  # fuse_env() returns the chunk evaluation environment
  out = md(text = c('```{r}', 'is.environment(fuse_env())', '```'))
  (grepl('#> [1] TRUE', out, fixed = TRUE))
  # get_context('format') returns the current output format inside fuse()
  out = md(text = c('```{r results="asis"}', 'cat(get_context("format"))', '```'))
  (grepl('markdown', out))
})

assert('engines() allows registering a custom block/inline engine used by fuse()', {
  old = engines(foo = function(x, inline = FALSE, ...) if (inline) 'INLINE' else 'BLOCK')
  on.exit(engines(old), add = TRUE)
  (grepl('BLOCK', md(text = c('```{foo}', 'anything', '```'))))
  (grepl('x INLINE z', md(text = 'x `{foo} y` z')))
})

assert('raw_text() wraps content in a raw block for a given format', {
  x = one_string(as.character(raw_text('<b>hi</b>', 'html')))
  (inherits(raw_text('<b>hi</b>', 'html'), 'record_asis'))
  (grepl('{=html}', x, fixed = TRUE))
  (grepl('<b>hi</b>', x, fixed = TRUE))
  # no format leaves the content unfenced
  (as.character(raw_text('plain')) %==% 'plain')
})

assert('fuse() renders raw_text() output verbatim (not as a code block)', {
  out = md(text = c('```{r results="asis"}', 'litedown::raw_text("<hr/>", "html")', '```'))
  (grepl('<hr/>', out, fixed = TRUE))
})
