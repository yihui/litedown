library(testit)

# helper: build a fake code chunk block
new_chunk = function(engine, source) {
  list(
    source = source, type = 'code_chunk',
    options = list(engine = engine, label = 'chunk-1'),
    lines = c(1L, length(source) + 2L), code_start = 2L
  )
}

assert('crack() returns an empty list for empty input', {
  (crack(character(0)) %==% list())
  # a single empty string becomes a text_block
  (length(crack('')) %==% 1L)
  (crack('')[[1]]$type %==% 'text_block')
})

assert('crack() identifies a code chunk followed by text', {
  src = c('```{r}', '1 + 1', '```', '', 'Hello world.')
  res = crack(text=src)
  (length(res) %==% 2L)
  (res[[1]]$type %==% 'code_chunk')
  (res[[2]]$type %==% 'text_block')
  (res[[1]]$options$engine %==% 'r')
  (res[[1]]$source %==% '1 + 1')
})

assert('crack() labels code chunks as chunk-1, chunk-2, ...', {
  src = c('```{r}', 'x = 1', '```', '', '```{r}', 'x + 1', '```')
  res = crack(text=src)
  chunks = Filter(function(b) b$type == 'code_chunk', res)
  (length(chunks) %==% 2L)
  (chunks[[1]]$options$label %==% 'chunk-1')
  (chunks[[2]]$options$label %==% 'chunk-2')
})

assert('crack() respects explicit chunk labels', {
  src = c('```{r my-label}', 'x = 1', '```')
  res = crack(text=src)
  (res[[1]]$options$label %==% 'my-label')
})

assert('crack() supports pipe-comment chunk options', {
  src = c('```{r}', '#| eval: false', 'x = 1', '```')
  res = crack(text=src)
  (isFALSE(res[[1]]$options$eval))
})

assert('crack() records line numbers of blocks', {
  src = c('```{r}', 'x = 1', '```', 'text')
  res = crack(text=src)
  (res[[1]]$lines %==% c(1L, 3L))
  (res[[2]]$lines %==% c(4L, 4L))
})

assert('crack() handles text-only input (no code chunks)', {
  res = crack(text=c('Hello', 'world'))
  (length(res) %==% 1L)
  (res[[1]]$type %==% 'text_block')
})

assert('crack() handles inline code in text blocks', {
  src = c('Value is `{r} 1 + 1`.')
  res = crack(text=src)
  (length(res) %==% 1L)
  (res[[1]]$type %==% 'text_block')
  # source is a list (not plain character) when inline code is present
  (is.list(res[[1]]$source))
})

assert('crack() strips fence padding for multi-backtick inline code on indented lines', {
  # `` `` ``-fences pad code with a space on each side; on an indented line,
  # commonmark's sourcepos ignores the indentation, so the leftover text used to
  # keep the fence and its padding space (yihui/litedown#132)
  src = c('- item', '  `` {r} 1 + 1 `` and `` {r} 2 + 2 `` done')
  res = crack(text=src)
  x = res[[1]]$source
  txt = unlist(x[!vapply(x, is.list, TRUE)])
  # no leftover backticks in the surrounding text fragments
  (!any(grepl('`', txt)))
  # the code fragments are extracted verbatim
  code = vapply(Filter(is.list, x), `[[`, '', 'source')
  (code %==% c('1 + 1', '2 + 2'))
})

assert('crack() supports non-R engines', {
  src = c('```{python}', 'x = 1', '```')
  res = crack(text=src)
  (res[[1]]$options$engine %==% 'python')
})

assert('crack() parses comma-separated chunk options in the header', {
  src = c('```{r, echo=TRUE, fig.width=7}', 'x = 1', '```')
  o = crack(text=src)[[1]]$options
  (o$echo %==% TRUE)
  (o$fig.width %==% 7)
  (o$engine %==% 'r')
})

assert('crack() keeps fences for double-brace (verbatim) chunks', {
  src = c('```{{r}}', 'verbatim', '```')
  b = crack(text=src)[[1]]
  (b$type %==% 'code_chunk')
  (b$source %==% 'verbatim')
  # the {{...}} header is unwrapped to a single-brace fence written to output
  (b$fences %==% c('```{r}', '```'))
  (b$options$engine %==% 'r')
})

assert('crack() records the prefix of an indented code chunk', {
  src = c('- item', '', '  ```{r}', '  1 + 1', '  ```')
  res = crack(text=src)
  chunk = res[[length(res)]]
  (chunk$type %==% 'code_chunk')
  (chunk$prefix %==% '  ')
  (chunk$source %==% '1 + 1')  # indentation stripped from the body
  (chunk$lines %==% c(3L, 5L))
})

assert('crack() records the prefix of a code chunk in a blockquote', {
  src = c('> ```{r}', '> 1 + 1', '> ```')
  b = crack(text=src)[[1]]
  (b$prefix %==% '> ')
  (b$source %==% '1 + 1')
})

assert('crack() supports tilde-fenced code chunks', {
  src = c('~~~{r}', '1 + 1', '~~~')
  b = crack(text=src)[[1]]
  (b$type %==% 'code_chunk')
  (b$source %==% '1 + 1')
  (b$options$engine %==% 'r')
})

assert('crack() handles a code chunk with an empty body', {
  src = c('```{r}', '```')
  b = crack(text=src)[[1]]
  (b$type %==% 'code_chunk')
  (length(b$source) %==% 0L)
})

assert('crack() does not treat plain fenced blocks (no braces) as code chunks', {
  # ```` ```python ```` (a language name, not `{...}`) is not a litedown chunk
  for (src in list(c('```python', 'y = 2', '```'), c('```{.python}', 'y = 2', '```'))) {
    res = crack(text=src)
    (length(res) %==% 1L)
    (res[[1]]$type %==% 'text_block')
  }
})

assert('crack() splits inline code that spans multiple lines', {
  src = c('wrap `{r}', 'x + 1` end')
  x = crack(text=src)[[1]]$source
  (is.list(x))
  code = Filter(is.list, x)
  (length(code) %==% 1L)
  (code[[1]]$source %==% 'x + 1')
  # position spans two source lines
  (code[[1]]$pos[1] %==% 1L)
  (code[[1]]$pos[3] %==% 2L)
})

assert('crack() flags inline code wrapped in $ $ as math', {
  x = crack(text='a $`{r} x`$ b')[[1]]$source
  code = Filter(is.list, x)[[1]]
  (isTRUE(code$math))
})

assert('crack() does not flag ordinary inline code as math', {
  x = crack(text='See `{r} pi` here.')[[1]]$source
  code = Filter(is.list, x)[[1]]
  (is.null(code$math))
})

assert('crack() handles multiple inline expressions and plain code spans', {
  x = crack(text='a `{r} x` b `{r} y + 1` c `plain` d')[[1]]$source
  code = Filter(is.list, x)
  # only the `{r} ...` spans are code; `plain` stays as text
  (length(code) %==% 2L)
  (vapply(code, `[[`, '', 'source') %==% c('x', 'y + 1'))
})

assert('crack() leaves a nested-backtick literal as text, not inline code', {
  # `` `{r} x` `` is a verbatim code span whose content is "`{r} x`" (with
  # backticks), so it is not a litedown inline expression
  x = crack(text='Use `` `{r} x` `` verbatim and `{r} y` real.')[[1]]$source
  code = Filter(is.list, x)
  (length(code) %==% 1L)
  (code[[1]]$source %==% 'y')
})

assert('crack() interleaves inline code across paragraphs of one text block', {
  src = c('para one `{r} a`', '', 'para two `{r} b`')
  x = crack(text=src)[[1]]$source
  code = Filter(is.list, x)
  (vapply(code, `[[`, '', 'source') %==% c('a', 'b'))
})

assert('sieve() returns list for R scripts', {
  src = c("x = 1", "x + 1")
  res = sieve(text=src)
  (is.list(res))
  (length(res) >= 1L)
  (res[[1]]$type %==% 'code_chunk')
  (res[[1]]$options$engine %==% 'r')
})

assert("sieve() extracts text blocks from lines starting with #'", {
  src = c("#' This is *doc*.", '', 'x = 1')
  res = sieve(text=src)
  types = vapply(res, `[[`, '', 'type')
  ('text_block' %in% types)
  ('code_chunk' %in% types)
  # the text block source contains the doc text
  tb = res[[which(types == 'text_block')]]
  (grepl('doc', tb$source))
})

assert('sieve() splits code by #| comments into separate chunks', {
  src = c('#| eval: false', 'x = 1', '#| echo: false', 'y = 2')
  res = sieve(text=src)
  (length(res) == 2L)
  (isFALSE(res[[1]]$options$eval))
  (isFALSE(res[[2]]$options$echo))
})

assert("sieve() handles pure code with no #| or #' markers", {
  src = c('x = 1', 'y = 2', 'x + y')
  res = sieve(text=src)
  # splits by expressions
  (length(res) >= 1L)
  all_code = all(vapply(res, function(b) b$type == 'code_chunk', TRUE))
  (all_code)
})
