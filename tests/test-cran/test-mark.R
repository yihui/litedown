library(testit)

assert('mark() with empty or trivial input produces empty output', {
  # character(0) gives a length-0 raw_string
  (length(mark(character(0))) %==% 0L)
  # empty string gives an empty raw_string
  (nchar(mark('')) %==% 0L)
  # empty file does not error and writes an empty HTML file
  f = tempfile()
  file.create(f)
  out = mark(f)
  (file_exists(out))
  (file.info(out)[, 'size'] %==% 0)
  unlink(c(f, out))
})

assert('mark() writes to a file when given a file path and returns it invisibly', {
  f = tempfile(fileext = '.html')
  ret = mark('Hello.', output = f)
  (file_exists(f))
  (ret %==% f)
  unlink(f)
})

assert('mark() treats I() input as text, not a file path', {
  out = mark(I('foo.md'), output = NA)
  (grepl('foo', out))
})

assert('mark() supports LaTeX and plain text output formats', {
  tex = mark('Hello _world_!', '.tex')
  (grepl('\\\\emph', tex))
  txt = mark('Hello _world_!', 'text')
  (grepl('Hello world', txt))
})

assert('mark() processes YAML metadata and applies title to output', {
  src = c('---', 'title: My Doc', '---', '', '# Hello')
  out = mark(I(src))
  (grepl('<title>My Doc</title>', out, fixed = TRUE))
})
