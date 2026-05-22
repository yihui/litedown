---
title: Plot files
---

The default extension for the `jpeg()` device is `jpeg`, and you can change it to `.jpg` if desired:

``` {.r}
plot(cars)
```
![](<010-option-plot-files__files/chunk-a-1.jpg>)

Set the plot size via `fig.dim`:

``` {.r}
plot(cars)
```
![](<010-option-plot-files__files/chunk-b-1.png>)

Write plot files to a different folder:

``` {.r}
plot(cars)
```
![](<figures/chunk-c-1.png>)
