---
title: Decorating figures
output:
  html:
    meta:
      css: ["@default", "@article"]
      js: ["@sidenotes"]
---

Place two plots side by side via the `width` attribute:

``` {.r}
hist(faithful$eruptions, main = '', border = 'white')
sunflowerplot(faithful)
```

:::: {.figure #fig:chunk-a}
![a histogram](<009-option-figure-decoration__files/chunk-a-1.png>){width="45%"}
![a sunflower plot](<009-option-figure-decoration__files/chunk-a-2.png>){width="45%"}


::: {.caption}
[](#@fig:chunk-a)
Exploring the faithful dataset.
:::
::::

A full-width figure (requires the `@article` CSS):

``` {.r}
par(mar = c(4, 4, .1, .1), bg = 'lightyellow', fg = 'red', las = 1)
plot(sunspots, col = 'red')
grid()
```

:::: {.figure .fullwidth #fig:chunk-b}
![Monthly mean relative sunspot numbers from 1749 to 1983.](<009-option-figure-decoration__files/chunk-b-1.png>)


::: {.caption}
[](#@fig:chunk-b)
Monthly mean relative sunspot numbers from 1749 to 1983.
:::
::::

Feel free to experiment with other class names provided by the `@article` CSS, such as `.side .side-right` or `.embed-right` in addition to `.fullwidth`.
