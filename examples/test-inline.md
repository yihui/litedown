---
title: "Test inline expressions"
---

A normal inline expression: 3.14.

The knitr-style inline expressions are ignored by default: `r pi`.

An expression that spans multiple lines: 3.14.

When a line has leading spaces that are not meaningful,
 it should still work: 3.14.

<!-- doesn't work when the next line has leading spaces, e.g., `{r} pi +
 1` -->

- An item.

  3.14 (leading spaces are meaningful here)
  
  hi FALSE
   hi 3.14
  
  <!-- doesn't work: hi `{r} pi +
  1` -->

      hi `{r} pi` (four spaces: this is a plain code block)
