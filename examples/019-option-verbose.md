---
title: Printing verbosity
---

Default `verbose = 0`:

``` {.r}
1:5  # a visible value
```

```
#> [1] 1 2 3 4 5
```

``` {.r}
x = 1 + 1  # invisible
for (i in 1:10) i^2  # for loop returns invisible NULL
y = x^2  # invisible
```

`verbose = 1` always prints the last value:

``` {.r}
1:5  # a visible value
```

```
#> [1] 1 2 3 4 5
```

``` {.r}
x = 1 + 1  # invisible
for (i in 1:10) i^2  # for loop returns invisible NULL
y = x^2  # invisible
```

```
#> [1] 4
```

`verbose = 2` prints all invisible values (except for `NULL`):

``` {.r}
1:5  # a visible value
```

```
#> [1] 1 2 3 4 5
```

``` {.r}
x = 1 + 1  # invisible
```

```
#> [1] 2
```

``` {.r}
for (i in 1:10) i^2  # for loop returns invisible NULL
y = x^2  # invisible
```

```
#> [1] 4
```
