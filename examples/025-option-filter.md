---
title: Filter results
---

Interleave plots with text output:

``` {.r}
for(i in 1:3) {
  cat('\n## ', LETTERS[i], '\n\n')
  plot(0, 0, pch = LETTERS[i])
}
```

##  A 

![](<025-option-filter__files/chunk-1-1.png>)

##  B 

![](<025-option-filter__files/chunk-1-2.png>)

##  C 

![](<025-option-filter__files/chunk-1-3.png>)
