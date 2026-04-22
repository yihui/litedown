# Snapshot tests for mark()

Basic Markdown rendering is verified below. The expected output blocks are generated and
compared on each test run.

## Basic inline formatting

```r
library(litedown)
mark('**bold** and _italic_')
```
```
<p><strong>bold</strong> and <em>italic</em></p>
```

## Headings

```r
mark(c('# H1', '', '## H2', '', '### H3'))
```
```
<h1 id="chp:h1">H1</h1>
<h2 id="sec:h2">H2</h2>
<h3 id="sec:h3">H3</h3>
```

## Unordered list

```r
mark(c('- item a', '- item b', '- item c'))
```
```
<ul>
<li>item a</li>
<li>item b</li>
<li>item c</li>
</ul>
```

## Ordered list

```r
mark(c('1. first', '2. second', '3. third'))
```
```
<ol>
<li>first</li>
<li>second</li>
<li>third</li>
</ol>
```

## Fenced code block with language attribute

```r
mark(c('```r', '1 + 1', '```'))
```
```
<pre><code class="language-r">1 + 1
</code></pre>
```

## Inline code

```r
mark('Use `x <- 1` to assign.')
```
```
<p>Use <code>x &lt;- 1</code> to assign.</p>
```

## Hyperlink

```r
mark('[example](https://example.com)')
```
```
<p><a href="https://example.com">example</a></p>
```

## Blockquote

```r
mark('> A wise quote.')
```
```
<blockquote>
<p>A wise quote.</p>
</blockquote>
```

## Horizontal rule

```r
mark(c('---'))
```
```
<hr />
```

## Table

```r
mark(c('| a | b |', '|---|---|', '| 1 | 2 |', '| 3 | 4 |'))
```
```
<table>
<thead>
<tr>
<th>a</th>
<th>b</th>
</tr>
</thead>
<tbody>
<tr>
<td>1</td>
<td>2</td>
</tr>
<tr>
<td>3</td>
<td>4</td>
</tr>
</tbody>
</table>
```

## Superscript and subscript

```r
mark('x^2^ and H~2~O')
```
```
<p>x<sup>2</sup> and H<sub>2</sub>O</p>
```

## Footnote

```r
mark(c('text[^1]', '', '[^1]: A footnote.'))
```
```
<p>text<sup class="footnote-ref"><a href="#fn-1" id="fnref-1" data-footnote-ref>1</a></sup></p>
<section class="footnotes" data-footnotes>
<ol>
<li id="fn-1">
<p>A footnote. <a href="#fnref-1" class="footnote-backref" data-footnote-backref data-footnote-backref-idx="1" aria-label="Back to reference 1">↩</a></p>
</li>
</ol>
</section>
```

## Raw HTML block

```r
mark(c('```{=html}', '<hr class="special">', '```'))
```
```
<hr class="special">
```

## LaTeX output format

```r
mark('Hello **world** and _litedown_.', '.tex')
```
```
Hello \textbf{world} and \emph{litedown}.
```

## Plain text output format

```r
mark('Hello **world**!', 'text')
```
```
Hello world!
```

## Smart quotes (smart option enabled by default)

```r
mark('"hello" and \'world\'')
```
```
<p>“hello” and ‘world’</p>
```
