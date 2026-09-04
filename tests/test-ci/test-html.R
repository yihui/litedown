library(testit)

# HTML rendering of multibyte (Chinese) input. Run only on CI (see
# tests/test-all.R) so that we exercise UTF-8 handling across operating
# systems (notably Windows), where multibyte string handling is most likely
# to differ. Each assert() renders a small snippet targeting one feature.

# mark() returns a raw_string (with a 'lang' attribute); strip it for %==%
md = function(...) as.character(mark(..., output = NA))

assert('Chinese prose and inline code survive in the HTML output', {
  (md(text = '圆的面积用 `面积` 表示。') %==%
    '<p>圆的面积用 <code>面积</code> 表示。</p>')
})

assert('Chinese in a code block survives', {
  (md(text = c('``` r', '斐波那契 <- function(n) n', '```')) %==%
    '<pre><code class="language-r">斐波那契 &lt;- function(n) n\n</code></pre>')
})

assert('a Chinese heading gets an auto identifier', {
  # alnum_id() drops non-ASCII, so the id falls back to the deduped 'sec:'
  (md(text = '## 简介') %==% '<h2 id="sec:">简介</h2>')
})

assert('number_sections numbers a Chinese heading', {
  (md(text = '## 简介', options = '+number_sections') %==%
    '<h2 id="sec:"><span class="section-number main-number">1</span> 简介</h2>')
})

assert('a cross-reference to a numbered Chinese section resolves', {
  (md(text = c('参见 @sec:intro。', '', '## 简介 {#sec:intro}'),
      options = '+number_sections') %==% paste(
    '<p>参见 <a class="cross-ref-sec" href="#sec:intro">1</a>。</p>',
    '<h2 id="sec:intro"><span class="section-number main-number">1</span> 简介</h2>',
    sep = '\n'
  ))
})

assert('smart punctuation is converted around Chinese text', {
  # -- -> en dash (U+2013), --- -> em dash (U+2014), ... -> ellipsis (U+2026)
  (md(text = '范围 1--10，步骤 2---3……等等...。') %==%
    '<p>范围 1–10，步骤 2—3……等等…。</p>')
})

assert('a fenced Div with a Chinese body renders', {
  (md(text = c('::: {.note}', '这是提示内容。', ':::')) %==%
    '<div class="note">\n<p>这是提示内容。</p>\n</div>')
})

assert('a table with Chinese headers and cells renders', {
  (md(text = c('|序号|数值|', '|--:|--:|', '|0|零|')) %==% paste(
    '<table>', '<thead>', '<tr>',
    '<th align="right">序号</th>', '<th align="right">数值</th>',
    '</tr>', '</thead>', '<tbody>', '<tr>',
    '<td align="right">0</td>', '<td align="right">零</td>',
    '</tr>', '</tbody>', '</table>',
    sep = '\n'
  ))
})

assert('a footnote with Chinese content renders', {
  # the footnote block carries generated ids/aria attributes, so match the
  # Chinese-bearing parts rather than the full HTML
  h = md(text = c('正文[^n]', '', '[^n]: 这是一个中文脚注。'))
  (matches(h, '.*<section class="footnotes".*') %==% '')
  (matches(h, '.*<li id="fn-n">\n<p>这是一个中文脚注。.*') %==% '')
})
