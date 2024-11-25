---
title: Collapse source code and text output
---

By default, the source and output are in separate blocks:


``` {.r}
x = 1 + 1
x
```

```
#> [1] 2
```

``` {.r}
x + 2
```

```
#> [1] 4
```

Set `collapse = TRUE` to collapse them:


``` {.r}
x = 1 + 1
x
#> [1] 2
x + 100
#> [1] 102
```
