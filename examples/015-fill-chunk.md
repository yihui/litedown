---
title: "Fill out references in code chunks"
---

Define `chunk-a` that includes a reference to `chunk-b`:







<!-- ... -->

Embed `chunk-a` in a function body:

``` {.r}
f = function() {
  n = 0
  for (i in 1:10000) {
    x = runif(1); y = runif(1)
    if (x^2 + y^2 <= 1) {
      n = n + 1
    }
  }
  n/10000 * 4
}
abs(f() - pi) <= 0.1  # is it close to pi?
```

```
#> [1] TRUE
```

We can generate code dynamically in any code chunk, not limited to R chunks, e.g.,

``` {.js}
const data = {
  "Sepal.Length": [5.1, 4.9, 4.7, 4.6, 5],
  "Sepal.Width": [3.5, 3, 3.2, 3.1, 3.6]
};
Object.keys(data);  // column names
```
