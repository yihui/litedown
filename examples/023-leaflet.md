---
title: "Leaflet"
---

Add the CSS/JS assets from the [leaflet](https://leafletjs.com) library:

``` {.r}
litedown::vest(css = '@npm/leaflet/dist/leaflet', js = '@npm/leaflet')
```
<link rel="stylesheet" href="https://cdn.jsdelivr.net/npm/leaflet/dist/leaflet.min.css">
<script src="https://cdn.jsdelivr.net/npm/leaflet" defer></script>

``` {.r}
loc = c(41.2542491, -95.9728748)
theta = (1:12)/6 * pi
circles = data.frame(Lat = loc[1] + sin(theta)/1000, Lng = loc[2] + cos(theta)/1000)
```

Provide a fenced Div with the ID `unmc` as the map container, and create the map:

::: {#unmc style="height: 500px;"}
:::

```` {.js}
const map = L.map('unmc').setView([41.2542491, -95.9728748], 17);

// add a tile layer
L.tileLayer('https://tile.openstreetmap.org/{z}/{x}/{y}.png', {
    maxZoom: 21,
    attribution: '&copy; <a href="http://www.openstreetmap.org/copyright">OpenStreetMap</a>'
}).addTo(map);

// add a marker and bind a popup to it
const marker = L.marker([41.2542491, -95.9728748]).addTo(map);
marker.bindPopup("<b>UNMC</b><p>You can play badminton here.</p>");

// draw a circle of circles (lat/long from the data frame `circles`)
[
  [41.2547491, -95.9720087745962],
  [41.2551151254038, -95.9723748],
  [41.2552491, -95.9728748],
  [41.2551151254038, -95.9733748],
  [41.2547491, -95.9737408254038],
  [41.2542491, -95.9738748],
  [41.2537491, -95.9737408254038],
  [41.2533830745962, -95.9733748],
  [41.2532491, -95.9728748],
  [41.2533830745962, -95.9723748],
  [41.2537491, -95.9720087745962],
  [41.2542491, -95.9718748]
].forEach(row => L.circle(row, {
  color: 'orangered', radius: 30,
  fillColor: 'lightskyblue', fillOpacity: 0.5
}).addTo(map));
````

``` {=html}
<script type="module">const map = L.map('unmc').setView([41.2542491, -95.9728748], 17);

// add a tile layer
L.tileLayer('https://tile.openstreetmap.org/{z}/{x}/{y}.png', {
    maxZoom: 21,
    attribution: '&copy; <a href="http://www.openstreetmap.org/copyright">OpenStreetMap</a>'
}).addTo(map);

// add a marker and bind a popup to it
const marker = L.marker([41.2542491, -95.9728748]).addTo(map);
marker.bindPopup("<b>UNMC</b><p>You can play badminton here.</p>");

// draw a circle of circles (lat/long from the data frame `circles`)
[
  [41.2547491, -95.9720087745962],
  [41.2551151254038, -95.9723748],
  [41.2552491, -95.9728748],
  [41.2551151254038, -95.9733748],
  [41.2547491, -95.9737408254038],
  [41.2542491, -95.9738748],
  [41.2537491, -95.9737408254038],
  [41.2533830745962, -95.9733748],
  [41.2532491, -95.9728748],
  [41.2533830745962, -95.9723748],
  [41.2537491, -95.9720087745962],
  [41.2542491, -95.9718748]
].forEach(row => L.circle(row, {
  color: 'orangered', radius: 30,
  fillColor: 'lightskyblue', fillOpacity: 0.5
}).addTo(map));</script>
```
