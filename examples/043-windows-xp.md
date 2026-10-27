---
title: "litedown with XP.css"
author: Yihui Xie
date: 2026-10-26
output:
  html:
    meta:
      css: ["@npm/xp.css/dist/XP.css", "reset.css"]
---

This page is styled by [XP.css](https://github.com/botoxparty/XP.css), which is loaded from a CDN via the `css` field
above. The page is plain Markdown: the window below is a
[fenced div](https://pkg.yihui.org/litedown/book/#sec:fenced-divs) (`:::`) whose classes
are the ones documented by XP.css.

::::: {.window  style="margin: 2em auto; max-width: 40em;"}
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
<p class="status-bar-field">XP.css</p>
:::
:::::
