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

Message blocks will be collapsed, too:

``` {.r}
nrow(iris)
message(ncol(iris))
#> 5
iris
```
