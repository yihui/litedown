# Snapshot tests for fuse()

## Basic code chunk execution

`````r
library(litedown)
fuse(text = c('```{r}', '1 + 1', '```'), output = 'markdown')
`````
`````
``` {.r}
1 + 1
```

```
#> [1] 2
```
`````

## Inline code evaluation

`````r
fuse(text = 'Value is `{r} 1 + 1`.', output = 'markdown')
`````
`````
Value is 2.
`````

## echo = FALSE hides source, shows output

`````r
fuse(text = c('```{r, echo=FALSE}', '2 * 3', '```'), output = 'markdown')
`````
`````
```
#> [1] 6
```
`````

## echo = -1 hides the first source line (#93)

`````r
fuse(text = c('```{r, echo=-1}', 'a = 1', 'b = 2', 'c = 3', '```'), output = 'markdown')
`````
`````
``` {.r}
b = 2
c = 3
```
`````

## echo = c(1, 3) shows only the selected source lines (#93)

`````r
fuse(text = c('```{r, echo=c(1, 3)}', 'a = 1', 'b = 2', 'c = 3', '```'), output = 'markdown')
`````
`````
``` {.r}
a = 1
c = 3
```
`````

## eval = FALSE doesn't run code

`````r
fuse(text = c('```{r, eval=FALSE}', 'stop("No")', '```'), output = 'markdown')
`````
`````
``` {.r}
stop("No")
```
`````

## results = FALSE suppresses output

`````r
fuse(text = c('```{r, results=FALSE}', '1 + 1', '```'), output = 'markdown')
`````
`````
``` {.r}
1 + 1
```
`````

## include = FALSE produces no output at all

`````r
fuse(text = c('```{r, include=FALSE}', '1 + 1', '```'), output = 'markdown')
`````
`````

`````

## comment option prefixes each output line

`````r
fuse(text = c('```{r, comment="##"}', '1:3', '```'), output = 'markdown')
`````
`````
``` {.r}
1:3
```

```
##[1] 1 2 3
```
`````

## comment = "" produces no prefix

`````r
fuse(text = c('```{r, comment=""}', '1:3', '```'), output = 'markdown')
`````
`````
``` {.r}
1:3
```

```
[1] 1 2 3
```
`````

## collapse = TRUE merges source and output blocks

`````r
fuse(text = c('```{r, collapse=TRUE}', '1 + 1', '2 + 2', '```'), output = 'markdown')
`````
`````
``` {.r}
1 + 1
#> [1] 2
2 + 2
#> [1] 4
```
`````

## results = "asis" output is written verbatim (no code fence)

`````r
fuse(text = c('#| results="asis"', 'cat("<p>hi</p>\\n")', '#| foo', '1:2'), output = 'markdown')
`````
`````
``` {.r}
cat("<p>hi</p>\n")
```
<p>hi</p>
``` {.r}
1:2
```

```
#> [1] 1 2
```
`````

## error = TRUE captures errors instead of stopping

`````r
fuse(text = c('```{r, error=TRUE}', 'stop("oops")', '```'), output = 'markdown')
`````
`````
``` {.r}
stop("oops")
```

``` {.plain .error}
#> Error: oops
```
`````

## warning = TRUE includes warnings in output

`````r
fuse(text = c('```{r, warning=TRUE}', 'warning("careful!")', '```'), output = 'markdown')
`````
`````
``` {.r}
warning("careful!")
```

``` {.plain .warning}
#> careful!
```
`````

## warning = FALSE suppresses warnings

`````r
fuse(text = c('```{r, warning=FALSE}', 'warning("shh")', '```'), output = 'markdown')
`````
`````
``` {.r}
warning("shh")
```
`````

## message = TRUE includes messages in output

`````r
fuse(text = c('```{r, message=TRUE}', 'message("hey")', '```'), output = 'markdown')
`````
`````
``` {.r}
message("hey")
```

``` {.plain .message}
#> hey
```
`````

## Multiple chunks with independent results

`````r
fuse(text = c('```{r}', 'x = 42', '```', '', '```{r}', 'x', '```'), output = 'markdown')
`````
`````
``` {.r}
x = 42
```

``` {.r}
x
```

```
#> [1] 42
```
`````

## Text-only input passes through unchanged

`````r
fuse(text = '# Title\n\nJust some text.', output = 'markdown')
`````
`````
# Title

Just some text.
`````

## Inline code: large numbers render as LaTeX math, others verbatim

`````r
fuse(text = 'Big `{r} 1e6`, small `{r} 42`, text `{r} paste0("a", "b")`.', output = 'markdown')
`````
`````
Big $10^{6}$, small 42, text ab.
`````

## A custom engine is used for both block and inline code

`````r
engines(foo = function(x, inline = FALSE, ...) if (inline) 'INLINE' else 'BLOCK')
fuse(text = c('```{foo}', 'ignored', '```', '', 'and `{foo} y` inline'), output = 'markdown')
`````
`````
```
#> BLOCK
```

and INLINE inline
`````

## fuse_env() and get_context() are available during evaluation

`````r
fuse(text = c('```{r}', 'is.environment(fuse_env())', 'get_context("format")', '```'), output = 'markdown')
`````
`````
``` {.r}
is.environment(fuse_env())
```

```
#> [1] TRUE
```

``` {.r}
get_context("format")
```

```
#> [1] "markdown"
```
`````

## results = "asis" with raw_text() emits verbatim HTML

`````r
fuse(text = c('```{r, results="asis"}', 'raw_text("<hr/>", "html")', '```'), output = 'markdown')
`````
`````
```` {.r}
raw_text("<hr/>", "html")
````

``` {=html}
<hr/>
```
`````
