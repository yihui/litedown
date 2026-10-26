---
title: "litedown with Pico CSS"
author: Yihui Xie
date: 2026-10-26
output:
  html:
    meta:
      css: ["https://cdn.jsdelivr.net/npm/@picocss/pico@2/css/pico.classless.min.css"]
    options:
      toc: true
---

This page is styled by [Pico CSS](https://picocss.com), loaded from a CDN with the `css` field
above. litedown's default stylesheet is not used.

Pico expects the page to be wrapped in `<main>`, but litedown uses `<div>`s, so
we center the content ourselves:

``` {=html}
<style type="text/css">
.frontmatter, .body {
  max-width: 800px;
  margin: auto;
  padding: 0 1rem;
}
</style>
```

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

``` {.r}
plot(cars)
```
![](<031-css-pico__files/chunk-2-1.png>)

## Table

``` {.r}
head(mtcars, 4)
```
| |mpg|cyl|disp|hp|drat|wt|qsec|vs|am|gear|carb|
|---|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|
|Mazda RX4|21.0|6|160|110|3.90|2.620|16.46|0|1|4|4|
|Mazda RX4 Wag|21.0|6|160|110|3.90|2.875|17.02|0|1|4|4|
|Datsun 710|22.8|4|108| 93|3.85|2.320|18.61|1|1|4|1|
|Hornet 4 Drive|21.4|6|258|110|3.08|3.215|19.44|1|0|3|1|

