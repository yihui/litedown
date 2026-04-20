old = litedown::reactor()
on.exit(litedown::reactor(old), add = TRUE)
litedown::reactor(fig.path = NULL, cache.path = NULL)

nested_markdown_src = '
```{r}
plot(2)
```
'
parent_markdown_src = '
```{r}
plot(1)
```

```{r}
litedown::fuse(text = nested_markdown_src, output = "markdown")
```
'

unlink('litedown__files', recursive = TRUE)
out = litedown::fuse(text = parent_markdown_src, output = 'markdown')
stopifnot(grepl('![](<litedown__files/chunk-1-1.png>)', out, fixed = TRUE))
stopifnot(file.exists('litedown__files/chunk-1-1.png'))
unlink('litedown__files', recursive = TRUE)
