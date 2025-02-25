---
title: "Simple DataTables"
---

Add the CSS/JS assets from the [simple-datatables](https://github.com/fiduswriter/simple-datatables/) library:

``` {.r}
litedown::vest(css = '@npm/simple-datatables/dist/style', js = '@npm/simple-datatables')
```
<link rel="stylesheet" href="https://cdn.jsdelivr.net/npm/simple-datatables/dist/style.min.css">
<script src="https://cdn.jsdelivr.net/npm/simple-datatables" defer></script>

There are two ways to render a table using the library. You can either pass the data as a JSON object:

```` {.js}
new simpleDatatables.DataTable('#table-iris', {
  data: {
    headings: ["Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width", "Species"],
    data: [
  [5.1, 3.5, 1.4, 0.2, "setosa"],
  [4.9, 3, 1.4, 0.2, "setosa"],
  [4.7, 3.2, 1.3, 0.2, "setosa"],
  [4.6, 3.1, 1.5, 0.2, "setosa"],
  [5, 3.6, 1.4, 0.2, "setosa"],
  [5.4, 3.9, 1.7, 0.4, "setosa"],
  [4.6, 3.4, 1.4, 0.3, "setosa"],
  [5, 3.4, 1.5, 0.2, "setosa"],
  [4.4, 2.9, 1.4, 0.2, "setosa"],
  [4.9, 3.1, 1.5, 0.1, "setosa"],
  [5.4, 3.7, 1.5, 0.2, "setosa"],
  [4.8, 3.4, 1.6, 0.2, "setosa"],
  [4.8, 3, 1.4, 0.1, "setosa"],
  [4.3, 3, 1.1, 0.1, "setosa"],
  [5.8, 4, 1.2, 0.2, "setosa"],
  [5.7, 4.4, 1.5, 0.4, "setosa"],
  [5.4, 3.9, 1.3, 0.4, "setosa"],
  [5.1, 3.5, 1.4, 0.3, "setosa"],
  [5.7, 3.8, 1.7, 0.3, "setosa"],
  [5.1, 3.8, 1.5, 0.3, "setosa"]
]
  },
  perPage: 5
});
````

``` {=html}
<script type="module">new simpleDatatables.DataTable('#table-iris', {
  data: {
    headings: ["Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width", "Species"],
    data: [
  [5.1, 3.5, 1.4, 0.2, "setosa"],
  [4.9, 3, 1.4, 0.2, "setosa"],
  [4.7, 3.2, 1.3, 0.2, "setosa"],
  [4.6, 3.1, 1.5, 0.2, "setosa"],
  [5, 3.6, 1.4, 0.2, "setosa"],
  [5.4, 3.9, 1.7, 0.4, "setosa"],
  [4.6, 3.4, 1.4, 0.3, "setosa"],
  [5, 3.4, 1.5, 0.2, "setosa"],
  [4.4, 2.9, 1.4, 0.2, "setosa"],
  [4.9, 3.1, 1.5, 0.1, "setosa"],
  [5.4, 3.7, 1.5, 0.2, "setosa"],
  [4.8, 3.4, 1.6, 0.2, "setosa"],
  [4.8, 3, 1.4, 0.1, "setosa"],
  [4.3, 3, 1.1, 0.1, "setosa"],
  [5.8, 4, 1.2, 0.2, "setosa"],
  [5.7, 4.4, 1.5, 0.4, "setosa"],
  [5.4, 3.9, 1.3, 0.4, "setosa"],
  [5.1, 3.5, 1.4, 0.3, "setosa"],
  [5.7, 3.8, 1.7, 0.3, "setosa"],
  [5.1, 3.8, 1.5, 0.3, "setosa"]
]
  },
  perPage: 5
});</script>
```

<table id="table-iris"></table>

or generate the data to an HTML table first:

::: {#mtcars}
| |mpg|cyl|disp|hp|drat|
|---|--:|--:|--:|--:|--:|
|Mazda RX4|21.0|6|160.0|110|3.90|
|Mazda RX4 Wag|21.0|6|160.0|110|3.90|
|Datsun 710|22.8|4|108.0| 93|3.85|
|Hornet 4 Drive|21.4|6|258.0|110|3.08|
|Hornet Sportabout|18.7|8|360.0|175|3.15|
|Valiant|18.1|6|225.0|105|2.76|
|Duster 360|14.3|8|360.0|245|3.21|
|Merc 240D|24.4|4|146.7| 62|3.69|
|Merc 230|22.8|4|140.8| 95|3.92|
|Merc 280|19.2|6|167.6|123|3.92|

:::

and then initialize a simple data table.

```` {.js}
new simpleDatatables.DataTable('#mtcars > table', {
  perPage: 1,
  perPageSelect: [1, 2, 4, 8, ['All', 0]]
});
````

``` {=html}
<script type="module">new simpleDatatables.DataTable('#mtcars > table', {
  perPage: 1,
  perPageSelect: [1, 2, 4, 8, ['All', 0]]
});</script>
```
