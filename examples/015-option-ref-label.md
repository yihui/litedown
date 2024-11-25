---
title: Reference chunks by labels
---

We define a function `abs2()`:


``` {.r}
abs2 = function(x)
```

Return `x` if `x >= 0`, otherwise return `-x`:


``` {.r}
ifelse(x >= 0, x, -x)
```

<!-- ... -->

The full function (combining `chunk-a` and `chunk-b`):


``` {.r}
abs2 = function(x)
  ifelse(x >= 0, x, -x)
```

See if it works:


``` {.r}
abs2(c(1, -2, 0, -100))
```

```
#> [1]   1   2   0 100
```
