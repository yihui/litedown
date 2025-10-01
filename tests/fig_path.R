src = '
---
output:
  html:
    options:
      embed_resources: false
---

```{r}
plot(1)
```
'

old = litedown::reactor(fig.path = 'foo')
out = litedown::fuse(text = src, output = 'markdown')
stopifnot('![](<foochunk-1-1.png>)' %in% unlist(strsplit(out, '\n')))
unlink('foochunk-1-1.png')

litedown::reactor(fig.path = 'foo/bar')
out = litedown::fuse(text = src, output = 'markdown')
stopifnot('![](<foo/barchunk-1-1.png>)' %in% unlist(strsplit(out, '\n')))
unlink('foo', recursive = TRUE)

litedown::reactor(old)
