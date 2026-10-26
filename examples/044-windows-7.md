---
title: "litedown with 7.css"
author: Yihui Xie
date: 2026-10-26
output:
  html:
    meta:
      css: ["@npm/7.css/dist/7.css"]
---

This page is styled by [7.css](https://github.com/khang-nd/7.css), which is loaded from a CDN via the `css` field
above. Nothing else is applied to the page: litedown's default stylesheet is not
used. The page is plain Markdown: the window below is a
[fenced div](https://pkg.yihui.org/litedown/book/#sec:fenced-divs) (`:::`) whose classes
are the ones documented by 7.css.

::::: {.window .glass .active style="margin: 2em auto; max-width: 40em;"}
::: title-bar
<div class="title-bar-text">litedown.md</div>
<div class="title-bar-controls">
<button aria-label="Minimize"></button>
<button aria-label="Maximize"></button>
<button aria-label="Close"></button>
</div>
:::

::: window-body
Plain text can be *emphasized*, **strong**, ~~deleted~~, or `inline code`, and
it can contain [links](https://yihui.org/litedown/).

> A block quote is a good place to quote someone.

1.  A numbered item.
2.  Another item.

-   A bullet item.
-   Another item.

| Package | Style      |
|:--------|:-----------|
| 98.css  | Windows 98 |
| XP.css  | Windows XP |
| 7.css   | Windows 7  |

<button class="default">OK</button> <button>Cancel</button>
:::

::: status-bar
<p class="status-bar-field">Rendered by litedown</p>
<p class="status-bar-field">7.css</p>
:::
:::::
