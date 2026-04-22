# Snapshot tests for get_context()

## get_context('format') returns the output format inside fuse()

```r
library(litedown)
fuse(text = c('```{r}', 'get_context("format")', '```'), output = 'markdown')
```
````
``` {.r}
get_context("format")
```

```
#> [1] "markdown"
```
````

## get_context('input') is NULL when fuse() uses text input (no file)

```r
fuse(text = c('```{r}', 'is.null(get_context("input"))', '```'), output = 'markdown')
```
````
``` {.r}
is.null(get_context("input"))
```

```
#> [1] TRUE
```
````
