---
title: Custom execution order
abstract: "We analyzed 14 8-cylinder cars, with an average MPG of 15.1."
---

Subset the data:


``` {.r}
n_cyl = 8
x = subset(mtcars, cyl == n_cyl)
```

The average MPG 15.1 is calculated from:


``` {.r}
m = mean(x$mpg)
```
