`results = 'hide'` should try to collapse output, e.g., merge the source blocks below:

``` {.r}
nrow(iris)
ncol(iris)
iris
```

Keep the blank line:

``` {.r}
nrow(iris)
ncol(iris)

iris
```

Other types of output (such as messages) will interrupt the collapsed block:

``` {.r}
nrow(iris)
message(ncol(iris))
```

``` {.plain .message}
#> 5
```

``` {.r}
iris
```
