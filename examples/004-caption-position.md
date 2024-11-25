---
title: Caption position
---

## Default caption positions

``` {.r}
plot(cars)
```

:::: {.figure #fig:fig-bottom}
![Bottom figure caption.](<004-caption-position__files/fig-bottom-1.png>)


::: {.fig-caption}
[](#@fig:fig-bottom)
Bottom figure caption.
:::
::::

``` {.r}
cars
```

:::: {.table #tab:tab-top}

::: {.tab-caption}
[](#@tab:tab-top)
Top table caption.
:::

|speed|dist|
|--:|--:|
| 4|  2|
| 4| 10|
| 7|  4|
| 7| 22|
| 8| 16|
|&vellip;|&vellip;|
|24| 70|
|24| 92|
|24| 93|
|24|120|
|25| 85|
::::


## Change the positions

``` {.r}
plot(cars)
```

:::: {.figure #fig:fig-top}

::: {.fig-caption}
[](#@fig:fig-top)
Top figure caption.
:::

![Top figure caption.](<004-caption-position__files/fig-top-1.png>)
::::

``` {.r}
cars
```

:::: {.table #tab:tab-bottom}
|speed|dist|
|--:|--:|
| 4|  2|
| 4| 10|
| 7|  4|
| 7| 22|
| 8| 16|
|&vellip;|&vellip;|
|24| 70|
|24| 92|
|24| 93|
|24|120|
|25| 85|


::: {.tab-caption}
[](#@tab:tab-bottom)
Bottom table caption.
:::
::::

