---
title: The `attr.*` chunk options
---

1. Add an ID `#example-a` to the whole chunk.

2. Add line numbers to source blocks via the `.line-numbers` class.

3. Add the class `.round` to the first plot and set its width to 400px.

4. Add two classes `.dark` and `.img-center` to the second plot.

::: {#example-a}

``` {.line-numbers}
plot(rnorm(100), rnorm(100))
```
![A scatterplot of rnorm(100) numbers.](<002-attr-options__files/example-a-1.png>){.round width="400"}

``` {.line-numbers}
i34 = iris[, 3:4]
smoothScatter(i34)
sunflowerplot(i34, add = TRUE)
```
![A sunflower plot of iris.](<002-attr-options__files/example-a-2.png>){.dark .img-center}
:::

Define CSS rules for the classes in the `#example-a` chunk:

```` {.css}
#example-a {
  .round { border: solid 1px; border-radius: 50%; }
  .dark  { filter: invert(1); }
  .img-center { display: block; margin: auto; }
}
````

``` {=html}
<style type="text/css">
#example-a {
  .round { border: solid 1px; border-radius: 50%; }
  .dark  { filter: invert(1); }
  .img-center { display: block; margin: auto; }
}
</style>
```
