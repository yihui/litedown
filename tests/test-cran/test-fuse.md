# Snapshot tests for fuse()

## Basic code chunk execution

````r
library(litedown)
fuse(text = c('```{r}', '1 + 1', '```'), output = 'markdown')
````
````
``` {.r}
1 + 1
```

```
#> [1] 2
```
````

## Inline code evaluation

````r
fuse(text = 'Value is `{r} 1 + 1`.', output = 'markdown')
````
````
Value is 2.
````

## echo = FALSE hides source, shows output

````r
fuse(text = c('```{r, echo=FALSE}', '2 * 3', '```'), output = 'markdown')
````
````
```
#> [1] 6
```
````

## eval = FALSE doesn't run code

````r
fuse(text = c('```{r, eval=FALSE}', 'stop("No")', '```'), output = 'markdown')
````
````
``` {.r}
stop("No")
```
````

## results = FALSE suppresses output

````r
fuse(text = c('```{r, results=FALSE}', '1 + 1', '```'), output = 'markdown')
````
````
``` {.r}
1 + 1
```
````

## include = FALSE produces no output at all

````r
fuse(text = c('```{r, include=FALSE}', '1 + 1', '```'), output = 'markdown')
````
````

````

## comment option prefixes each output line

````r
fuse(text = c('```{r, comment="##"}', '1:3', '```'), output = 'markdown')
````
````
``` {.r}
1:3
```

```
##[1] 1 2 3
```
````

## comment = "" produces no prefix

````r
fuse(text = c('```{r, comment=""}', '1:3', '```'), output = 'markdown')
````
````
``` {.r}
1:3
```

```
[1] 1 2 3
```
````

## collapse = TRUE merges source and output blocks

````r
fuse(text = c('```{r, collapse=TRUE}', '1 + 1', '2 + 2', '```'), output = 'markdown')
````
````
``` {.r}
1 + 1
#> [1] 2
2 + 2
#> [1] 4
```
````

## results = "asis" output is written verbatim (no code fence)

````r
fuse(text = c('#| results="asis"', 'cat("<p>hi</p>\\n")', '#| foo', '1:2'), output = 'markdown')
````
````
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
````

## error = TRUE captures errors instead of stopping

````r
fuse(text = c('```{r, error=TRUE}', 'stop("oops")', '```'), output = 'markdown')
````
````
``` {.r}
stop("oops")
```

``` {.plain .error}
#> Error: oops
```
````

## warning = TRUE includes warnings in output

````r
fuse(text = c('```{r, warning=TRUE}', 'warning("careful!")', '```'), output = 'markdown')
````
````
``` {.r}
warning("careful!")
```

``` {.plain .warning}
#> careful!
```
````

## warning = FALSE suppresses warnings

````r
fuse(text = c('```{r, warning=FALSE}', 'warning("shh")', '```'), output = 'markdown')
````
````
``` {.r}
warning("shh")
```
````

## message = TRUE includes messages in output

````r
fuse(text = c('```{r, message=TRUE}', 'message("hey")', '```'), output = 'markdown')
````
````
``` {.r}
message("hey")
```

``` {.plain .message}
#> hey
```
````

## Multiple chunks with independent results

````r
fuse(text = c('```{r}', 'x = 42', '```', '', '```{r}', 'x', '```'), output = 'markdown')
````
````
``` {.r}
x = 42
```

``` {.r}
x
```

```
#> [1] 42
```
````

## Text-only input passes through unchanged

````r
fuse(text = '# Title\n\nJust some text.', output = 'markdown')
````
````
# Title

Just some text.
````
