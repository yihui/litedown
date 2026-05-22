---
title: The `code` option
---

Define a code template `tpl`:

``` {.r}
tpl    = 'lm(mpg ~ %s, data = mtcars) |> summary() |> coef()'
x_vars = names(mtcars)[2:4]
```

We run regressions on three variables one by one:

<!-- ... -->

``` {.r}
lm(mpg ~ cyl, data = mtcars) |> summary() |> coef()
```
| |Estimate|Std. Error|t value|Pr(>\|t\|)|
|---|--:|--:|--:|--:|
|(Intercept)|37.885| 2.074|18.268| 0.000|
|cyl|-2.876| 0.322|-8.920| 0.000|


``` {.r}
lm(mpg ~ disp, data = mtcars) |> summary() |> coef()
```
| |Estimate|Std. Error|t value|Pr(>\|t\|)|
|---|--:|--:|--:|--:|
|(Intercept)|29.600| 1.230|24.070| 0.000|
|disp|-0.041| 0.005|-8.747| 0.000|


``` {.r}
lm(mpg ~ hp, data = mtcars) |> summary() |> coef()
```
| |Estimate|Std. Error|t value|Pr(>\|t\|)|
|---|--:|--:|--:|--:|
|(Intercept)|30.099| 1.634|18.421| 0.000|
|hp|-0.068| 0.010|-6.742| 0.000|

