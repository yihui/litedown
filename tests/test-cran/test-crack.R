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

assert('crack() supports non-R engines', {
  src = c('```{python}', 'x = 1', '```')
  res = crack(text=src)
  (res[[1]]$options$engine %==% 'python')
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
