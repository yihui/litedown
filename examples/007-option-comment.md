---
title: The `comment` option
---

Use `#-> ` to comment out text output:


``` {.r}
matrix(1:12, 3)
```

```
#->      [,1] [,2] [,3] [,4]
#-> [1,]    1    4    7   10
#-> [2,]    2    5    8   11
#-> [3,]    3    6    9   12
```

Do not comment out text output:


``` {.r}
matrix(1:12, 3)
```

```
     [,1] [,2] [,3] [,4]
[1,]    1    4    7   10
[2,]    2    5    8   11
[3,]    3    6    9   12
```
