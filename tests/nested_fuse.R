old = litedown::reactor()
on.exit(litedown::reactor(old), add = TRUE)
litedown::reactor(fig.path = NULL, cache.path = NULL)

src_inner = '
```{r}
plot(2)
```
'
src_outer = '
```{r}
plot(1)
```

```{r}
litedown::fuse(text = src_inner, output = "markdown")
```
'

unlink('litedown__files', recursive = TRUE)
out = litedown::fuse(text = src_outer, output = 'markdown')
stopifnot(grepl('![](<litedown__files/chunk-1-1.png>)', out, fixed = TRUE))
stopifnot(file.exists('litedown__files/chunk-1-1.png'))
unlink('litedown__files', recursive = TRUE)
