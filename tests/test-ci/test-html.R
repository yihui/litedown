library(testit)

# HTML rendering of multibyte (Chinese) input. Run only on CI (see
# tests/test-all.R) so that we exercise UTF-8 handling across operating
# systems (notably Windows), where multibyte string handling is most likely
# to differ. Each assert() renders a small snippet targeting one feature.

# mark() returns a raw_string (with a 'lang' attribute); strip it for %==%
md = function(...) as.character(mark(text = c(...), output = NA))
options(litedown.jsd_resolve = FALSE)

assert('Chinese prose and inline code survive in the HTML output', {
  h = md('圆的面积用 `面积` 表示。')
  (matches(h, '.*圆的面积用 <code>面积</code> 表示。.*') %==% '')
})

assert('Chinese in a code block survives', {
  h = md('``` r', '斐波那契 <- function(n) n', '```')
  (matches(h, '.*斐波那契 &lt;- function.*') %==% '')
})

assert('a Chinese heading gets an auto identifier', {
  # alnum_id() drops non-ASCII, so the id falls back to the deduped 'sec:'
  h = md('## 简介')
  (matches(h, '.*<h2 id="sec:">.*简介</h2>.*') %==% '')
})

assert('number_sections numbers a Chinese heading', {
  h = as.character(mark(text = '## 简介', output = NA, options = '+number_sections'))
  (matches(h, '.*class="section-number main-number">1</span> 简介.*') %==% '')
})

assert('a cross-reference to a numbered Chinese section resolves', {
  h = as.character(mark(
    text = c('参见 @sec:intro。', '', '## 简介 {#sec:intro}'),
    output = NA, options = '+number_sections'
  ))
  (matches(h, '.*href="#sec:intro".*') %==% '')
  (matches(h, '.*id="sec:intro".*') %==% '')
})

assert('smart punctuation is converted around Chinese text', {
  # dashes and ellipsis convert next to CJK: -- -> en dash (U+2013),
  # --- -> em dash (U+2014), ... -> ellipsis (U+2026)
  h = md('范围 1--10，步骤 2---3……等等...。')
  (matches(h, '.*范围 1–10，步骤 2—3……等等…。.*') %==% '')
})

assert('a footnote with Chinese content renders', {
  h = md('正文[^n]', '', '[^n]: 这是一个中文脚注。')
  (matches(h, '.*class="footnotes".*') %==% '')
  (matches(h, '.*这是一个中文脚注。.*') %==% '')
})

assert('a fenced Div with a Chinese body renders', {
  h = md('::: {.note}', '这是提示内容。', ':::')
  (matches(h, '.*<div class="note">.*这是提示内容。.*</div>.*') %==% '')
})

assert('a table with Chinese headers and cells renders', {
  h = md('|序号|数值|', '|--:|--:|', '|0|零|')
  (matches(h, '.*<th[^>]*>序号</th>.*') %==% '')
  (matches(h, '.*<td[^>]*>零</td>.*') %==% '')
})
