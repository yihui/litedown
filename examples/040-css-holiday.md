---
title: "litedown with Holiday.css"
author: Yihui Xie
date: 2026-10-26
output:
  html:
    meta:
      css: ["@npm/holiday.css/dist/holiday.css"]
    options:
      toc: true
---

This page is styled by [Holiday.css](https://holidaycss.js.org), which is loaded from a CDN via the `css` field
above. Nothing else is applied to the page: litedown's default stylesheet is not
used.

## Text

Plain text can be *emphasized*, **strong**, ~~deleted~~, or `inline code`, and
it can contain [links](https://yihui.org/litedown/) and footnotes.[^1]

> A block quote is a good place to quote someone.

1.  A numbered item.
2.  Another item.

-   A bullet item.
-   Another item.

[^1]: A footnote.

## Code and output

``` {.r}
summary(cars)
```

```
#>      speed           dist       
#>  Min.   : 4.0   Min.   :  2.00  
#>  1st Qu.:12.0   1st Qu.: 26.00  
#>  Median :15.0   Median : 36.00  
#>  Mean   :15.4   Mean   : 42.98  
#>  3rd Qu.:19.0   3rd Qu.: 56.00  
#>  Max.   :25.0   Max.   :120.00  
```

## Diagram

:::: {.figure #fig:chunk-2}

``` pele
flowchart LR
  A@{ shape: doc, label: "*.Rmd" }-. <code>fuse()</code> .-> B@{ shape: doc, label: "*.md" }
  B-- <code>mark()</code> -->C@{ shape: doc, label: "*.html" }
  D@{ shape: cyl, label: "CSS (CDN)" }-. <code>css</code> .->C
  C==>E@{ shape: stadium, label: "Styled page" }
```


::: {.caption}
[](#@fig:chunk-2)
How an R Markdown document becomes a styled HTML page.
:::
::::

## Table

``` {.r}
head(mtcars, 4)
```

:::: {.table #tab:chunk-3}

::: {.caption}
[](#@tab:chunk-3)
First 4 rows of the `mtcars` data.
:::

| |mpg|cyl|disp|hp|drat|wt|qsec|vs|am|gear|carb|
|---|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|
|Mazda RX4|21.0|6|160|110|3.90|2.620|16.46|0|1|4|4|
|Mazda RX4 Wag|21.0|6|160|110|3.90|2.875|17.02|0|1|4|4|
|Datsun 710|22.8|4|108| 93|3.85|2.320|18.61|1|1|4|1|
|Hornet 4 Drive|21.4|6|258|110|3.08|3.215|19.44|1|0|3|1|
::::

