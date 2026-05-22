`collapse = TRUE` will collapse messages into source blocks, too.

``` {.r}
message("this is a message")
#> this is a message

warning("this is a warning")
#> this is a warning

stop("this is an error")
#> Error: this is an error
```

Not collapsing or striping blank lines:

``` {.r}
message("this is a message")
```

``` {.plain .message}
#> this is a message
```

``` {.r}
warning("this is a warning")
```

``` {.plain .warning}
#> this is a warning
```

``` {.r}
stop("this is an error")
```

``` {.plain .error}
#> Error: this is an error
```

Collapsing without striping blank lines:

``` {.r}
message("this is a message")
#> this is a message
warning("this is a warning")
#> this is a warning
stop("this is an error")
#> Error: this is an error
```
