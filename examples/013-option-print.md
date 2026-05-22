---
title: Print objects
---

Print objects with `base::print()`, and use different arguments for different objects.

``` {.r}
X = c('a', 'b', 'c', 'c', 'c', 'a')
Y = factor(c('A', 'B', 'C', 'C', 'D', 'E'))
Y  # factor
```

```
#> [1] "A" "B" "C" "C" "D" "E"
#> 5 Levels: "A" "B" ... "E"
```

``` {.r}
table(X, Y)  # table
```

```
#>    Y
#> X   A B C D E
#>   a 1 . . . 1
#>   b . 1 . . .
#>   c . . 2 1 .
```
