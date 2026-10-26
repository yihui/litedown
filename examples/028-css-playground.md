---
title: Customize the look of a page
subtitle: Tweak the major CSS settings and copy the CSS code
output:
  html:
    meta:
      css: ["@default"]
    options:
      toc: true
---

Change the settings below, and the page will update right away. Only the
settings you have changed appear in the CSS code, which you can copy into a CSS
chunk in your document, or into a `.css` file listed in the `css` field of the
YAML metadata. See
[Custom styles](https://pkg.yihui.org/litedown/book/#sec:custom-styles) in the
book for details.

``` {=html}
<style type="text/css">
#ui {
  display: grid;
  grid-template-columns: repeat(auto-fit, minmax(13em, 1fr));
  gap: .5em 1em;
  border: 1px solid #ddd;
  padding: 1em;
  label { display: flex; flex-direction: column; font-size: .9em; }
  input, select { width: 100%; }
}
</style>
```

## Settings

<form id="ui">
<label>Font family
<select data-sel="body" data-prop="font-family">
<option value="">Default</option>
<option value="Georgia, serif">Serif</option>
<option value="system-ui, sans-serif">System UI</option>
<option value="ui-monospace, monospace">Monospace</option>
</select></label>
<label>Font size (px)
<input type="range" min="12" max="24" data-sel="body" data-prop="font-size" data-unit="px"></label>
<label>Line height
<input type="range" min="1" max="2.2" step=".1" data-sel="body" data-prop="line-height"></label>
<label>Page width (px)
<input type="range" min="500" max="1200" step="20" data-sel="body" data-prop="max-width" data-unit="px"></label>
<label>Text color
<input type="color" data-sel="body" data-prop="color"></label>
<label>Background color
<input type="color" data-sel="body" data-prop="background"></label>
<label>Link color
<input type="color" data-sel="a" data-prop="color"></label>
<label>Heading color
<input type="color" data-sel="h1, h2, h3" data-prop="color" data-read="h2"></label>
<p><button type="reset">Reset</button> <button type="button" id="copy">Copy CSS</button></p>
</form>

## CSS code

<pre><code id="out" class="language-css"></code></pre>

## Preview

This section shows how some common elements look. Here is a
[link](https://yihui.org/litedown/), some `inline code`, and a quote:

> A block quote is a good place to quote someone.

``` {.r}
summary(cars$speed)
```

```
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#>     4.0    12.0    15.0    15.4    19.0    25.0 
```

``` {.r}
head(cars, 3)
```

:::: {.table #tab:chunk-3}

::: {.caption}
[](#@tab:chunk-3)
First 3 rows of the `cars` data.
:::

|speed|dist|
|--:|--:|
|4| 2|
|4|10|
|7| 4|
::::


``` {=html}
<script type="module">const d = document, ui = d.getElementById("ui"), out = d.getElementById("out"),
  inputs = [...ui.querySelectorAll("[data-prop]")],
  style = d.head.appendChild(d.createElement("style")),
  hex = c => c === "rgba(0, 0, 0, 0)" ? "#ffffff" :
    "#" + c.match(/\d+/g).slice(0, 3).map(i => (+i).toString(16).padStart(2, "0")).join(""),
  // read the initial value of a setting from the computed style
  init = {
    "font-size": cs => parseFloat(cs.fontSize),
    "line-height": cs => +(parseFloat(cs.lineHeight) / parseFloat(cs.fontSize)).toFixed(1),
    "max-width": cs => parseFloat(cs.maxWidth),
    color: cs => hex(cs.color),
    background: cs => hex(cs.backgroundColor)
  };

inputs.forEach(el => {
  const v = init[el.dataset.prop]?.(getComputedStyle(d.querySelector(el.dataset.read ?? el.dataset.sel)));
  if (v !== undefined) el.value = v;
  el.dataset.init = el.value;
});

// show only the settings that have been changed
const update = () => {
  const rules = {};
  inputs.forEach(el => el.value !== el.dataset.init &&
    (rules[el.dataset.sel] ??= []).push(`  ${el.dataset.prop}: ${el.value}${el.dataset.unit ?? ""};`));
  style.textContent = Object.entries(rules).map(([s, v]) => `${s} {\n${v.join("\n")}\n}`).join("\n");
  out.textContent = style.textContent || "/* Change a setting above to generate CSS. */";
};
// sliders update on release (`change`) so the page doesn't shift while dragging
ui.oninput = e => e.target.type !== "range" && update();
ui.onchange = e => e.target.type === "range" && update();
ui.onreset = () => setTimeout(() => {
  inputs.forEach(el => el.value = el.dataset.init);  // not the HTML defaults
  update();
});
d.getElementById("copy").onclick = () => navigator.clipboard.writeText(style.textContent);
update();</script>
```
