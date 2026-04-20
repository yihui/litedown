old = litedown::reactor()
on.exit(litedown::reactor(old), add = TRUE)
litedown::reactor(fig.path = NULL, cache.path = NULL)

inner_document = '
```{r}
plot(2)
```
'
outer_document = '
```{r}
plot(1)
```

```{r}
litedown::fuse(text = inner_document, output = "markdown")
```
'

unlink('litedown__files', recursive = TRUE)
stopifnot(!dir.exists('litedown__files'))
out = litedown::fuse(text = outer_document, output = 'markdown')
stopifnot(grepl('![](<litedown__files/chunk-1-1.png>)', out, fixed = TRUE))
stopifnot(file.exists('litedown__files/chunk-1-1.png'))
unlink('litedown__files', recursive = TRUE)
