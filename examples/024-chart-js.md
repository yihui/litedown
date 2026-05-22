---
title: "Chart.js"
---

Import the [chart.js](https://www.chartjs.org) library:

``` {.r}
litedown::vest(js = '@npm/chart.js')
```
<script src="https://cdn.jsdelivr.net/npm/chart.js" defer></script>

Draw the chart with the `uspop` dataset:

```` {.js}
const ctx = document.getElementById('uspop-polar');
new Chart(ctx, {
  type: 'polarArea',
  data: {
    labels: [1790, 1800, 1810, 1820, 1830, 1840, 1850, 1860, 1870, 1880, 1890, 1900, 1910, 1920, 1930, 1940, 1950, 1960, 1970],
    datasets: [{
      data: [3.93, 5.31, 7.24, 9.64, 12.9, 17.1, 23.2, 31.4, 39.8, 50.2, 62.9, 76, 92, 105.7, 122.8, 131.7, 151.3, 179.3, 203.2]
    }]
  },
  options: {
    responsive: true,
    scales: {
      r: {
        pointLabels: {
          display: true,
          centerPointLabels: true
        }
      }
    },
    plugins: {
      legend: {
        display: false
      }
    }
  }
});
````

``` {=html}
<script type="module">const ctx = document.getElementById('uspop-polar');
new Chart(ctx, {
  type: 'polarArea',
  data: {
    labels: [1790, 1800, 1810, 1820, 1830, 1840, 1850, 1860, 1870, 1880, 1890, 1900, 1910, 1920, 1930, 1940, 1950, 1960, 1970],
    datasets: [{
      data: [3.93, 5.31, 7.24, 9.64, 12.9, 17.1, 23.2, 31.4, 39.8, 50.2, 62.9, 76, 92, 105.7, 122.8, 131.7, 151.3, 179.3, 203.2]
    }]
  },
  options: {
    responsive: true,
    scales: {
      r: {
        pointLabels: {
          display: true,
          centerPointLabels: true
        }
      }
    },
    plugins: {
      legend: {
        display: false
      }
    }
  }
});</script>
```

<canvas id="uspop-polar"></canvas>
