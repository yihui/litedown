library(testit)

assert('mark() with empty or trivial input produces empty output', {
  # character(0) gives a length-0 raw_string
  (length(litedown::mark(character(0))) %==% 0L)
  # empty string gives an empty raw_string
  (nchar(litedown::mark('')) %==% 0L)
  # empty file does not error and writes an empty HTML file
  f = tempfile()
  if (file.create(f)) {
    out = litedown::mark(f)
    (file_exists(out))
    (nchar(xfun::read_utf8(out)) %==% 0L)
    unlink(c(f, out))
  }
})

assert('mark() writes to a file when given a file path and returns it invisibly', {
  f = tempfile(fileext = '.html')
  ret = litedown::mark('Hello.', output = f)
  (file_exists(f))
  (ret %==% f)
  unlink(f)
})

assert('mark() treats I() input as text, not a file path', {
  out = litedown::mark(I('foo.md'), output = NA)
  (grepl('foo', out))
})

assert('mark() supports LaTeX and plain text output formats', {
  tex = litedown::mark('Hello _world_!', '.tex')
  (grepl('\\\\emph', tex))
  txt = litedown::mark('Hello _world_!', 'text')
  (grepl('Hello world', txt))
})

assert('mark() processes YAML metadata and applies title to output', {
  src = c('---', 'title: My Doc', '---', '', '# Hello')
  out = litedown::mark(I(src), output = NA)
  (grepl('<title>My Doc</title>', out, fixed = TRUE))
})

assert('fiss() extracts R code from an R Markdown document', {
  src = c('```{r}', 'x = 1', '```', 'text', '```{r}', 'x + 1', '```')
  out = litedown::fiss(I(src), output = NA)
  (any(grepl('x = 1', out)))
  (any(grepl('x \\+ 1', out)))
})

assert('fiss() respects purl = FALSE chunk option', {
  src = c('```{r, purl=FALSE}', 'secret = 1', '```', '```{r}', 'public = 2', '```')
  out = litedown::fiss(I(src), output = NA)
  (!any(grepl('secret', out)))
  (any(grepl('public', out)))
})

