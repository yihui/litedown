---
title: Create a callout via the option `attr.chunk`
output:
  litedown::html_format:
    meta:
      css: ["@default", "@callout"]
      js: ["@callout"]
---

If you use the class name `.callout-*` on a chunk, you can turn it into a callout, e.g.,

::: {.callout-example}

``` {.r}
1 + 1
```

```
#> [1] 2
```
:::

Remember to load the `callout` CSS/JS assets in YAML.
