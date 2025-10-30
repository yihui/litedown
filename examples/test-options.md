## Test Markdown options

``` {.r}
library(litedown)
```

````` {.r}
# toc example
mkd <- c('# Header 1', 'p1', '## Header 2', 'p2')

mark(mkd, options = '+number_sections')
`````

````` {.html .plain}
<h1 id="chp:header-1"><span class="section-number main-number">1</span> Header 1</h1>
<p>p1</p>
<h2 id="sec:header-2"><span class="section-number">1.1</span> Header 2</h2>
<p>p2</p>
`````

````` {.r}
mark(mkd, options = '+number_sections+toc')
`````

````` {.html .plain}
<div id="TOC">
<ul class="numbered">
<li><a href="#chp:header-1"><span class="section-number main-number">1</span> Header 1</a>
<ul>
<li><a href="#sec:header-2"><span class="section-number">1.1</span> Header 2</a></li>
</ul>
</li>
</ul>
</div>
<h1 id="chp:header-1"><span class="section-number main-number">1</span> Header 1</h1>
<p>p1</p>
<h2 id="sec:header-2"><span class="section-number">1.1</span> Header 2</h2>
<p>p2</p>
`````

````` {.r}
# hard_wrap example
mark('foo\nbar\n')
`````

````` {.html .plain}
<p>foo
bar</p>
`````

````` {.r}
mark('foo\nbar\n', options = '+hardbreaks')
`````

````` {.html .plain}
<p>foo<br />
bar</p>
`````

````` {.r}
# latex math example
mkd <- c(
  '`$x$` is inline math $x$!', '', 'Display style:', '', '$$x + y$$', '',
  '\\begin{align}
a^{2}+b^{2} & = c^{2}\\\\
\\sin^{2}(x)+\\cos^{2}(x) & = 1
\\end{align}'
)

mark(mkd)
`````

````` {.html .plain}
<p><code>$x$</code> is inline math \(x\)!</p>
<p>Display style:</p>
<p>$$x + y$$</p>
<p>\begin{align}
a^{2}+b^{2} &amp; = c^{2}\\
\sin^{2}(x)+\cos^{2}(x) &amp; = 1
\end{align}</p>
`````

````` {.r}
mark(mkd, options = '-latex_math')
`````

````` {.html .plain}
<p><code>$x$</code> is inline math $x$!</p>
<p>Display style:</p>
<p>$$x + y$$</p>
<p>\begin{align}
a^{2}+b^{2} &amp; = c^{2}\
\sin^{2}(x)+\cos^{2}(x) &amp; = 1
\end{align}</p>
`````

````` {.r}
# table example
mark('
First Header  | Second Header
------------- | -------------
Content Cell  | Content Cell
Content Cell  | Content Cell
')
`````

````` {.html .plain}
<table>
<thead>
<tr>
<th>First Header</th>
<th>Second Header</th>
</tr>
</thead>
<tbody>
<tr>
<td>Content Cell</td>
<td>Content Cell</td>
</tr>
<tr>
<td>Content Cell</td>
<td>Content Cell</td>
</tr>
</tbody>
</table>
`````

````` {.r}
# caption
mark('
| a | b |
|---|--:|
| A | 9 |

Table: A table _caption_.
')
`````

````` {.html .plain}
<table>
<caption>A table <em>caption</em>.</caption>
<thead>
<tr>
<th>a</th>
<th align="right">b</th>
</tr>
</thead>
<tbody>
<tr>
<td>A</td>
<td align="right">9</td>
</tr>
</tbody>
</table>
`````

````` {.r}
# no table
mark('
First Header  | Second Header
------------- | -------------
Content Cell  | Content Cell
Content Cell  | Content Cell
', options = '-table')
`````

````` {.html .plain}
<p>First Header  | Second Header
———–– | ———––
Content Cell  | Content Cell
Content Cell  | Content Cell</p>
`````

````` {.r}
# autolink example
mark('https://www.r-project.org/')
`````

````` {.html .plain}
<p><a href="https://www.r-project.org/">https://www.r-project.org/</a></p>
`````

````` {.r}
mark('https://www.r-project.org/', options = '-autolink')
`````

````` {.html .plain}
<p>https://www.r-project.org/</p>
`````

````` {.r}
# links and spans
mark('[a b](#){.red}')
`````

````` {.html .plain}
<p><a href="#" class="red">a b</a></p>
`````

````` {.r}
mark('[a\nb](){.red}')
`````

````` {.html .plain}
<p><span class="red">a
b</span></p>
`````

````` {.r}
# strikethrough example
mark('~~awesome~~')
`````

````` {.html .plain}
<p><del>awesome</del></p>
`````

````` {.r}
mark('~~awesome~~', options = '-strikethrough')
`````

````` {.html .plain}
<p>~~awesome~~</p>
`````

````` {.r}
# superscript and subscript examples
mark('2^10^')
`````

````` {.html .plain}
<p>2<sup>10</sup></p>
`````

````` {.r}
mark('2^10^', options = '-superscript')
`````

````` {.html .plain}
<p>2^10^</p>
`````

````` {.r}
mark('H~2~O')
`````

````` {.html .plain}
<p>H<sub>2</sub>O</p>
`````

````` {.r}
mark('H~2~O', options = '-subscript')
`````

````` {.html .plain}
<p>H~2~O</p>
`````

````` {.r}
# code blocks
mark('```r\n1 + 1;\n```')
`````

````` {.html .plain}
<pre><code class="language-r">1 + 1;
</code></pre>
`````

````` {.r}
mark('```{.r}\n1 + 1;\n```')
`````

````` {.html .plain}
<pre><code class="language-r">1 + 1;
</code></pre>
`````

````` {.r}
mark('```{.r .js}\n1 + 1;\n```')
`````

````` {.html .plain}
<pre><code class="language-r js">1 + 1;
</code></pre>
`````

````` {.r}
mark('```{.r .js #foo}\n1 + 1;\n```')
`````

````` {.html .plain}
<pre><code class="language-r js" id="foo">1 + 1;
</code></pre>
`````

````` {.r}
mark('```{.r .js #foo style="background:lime;"}\n1 + 1;\n```')
`````

````` {.html .plain}
<pre><code class="language-r js" id="foo" style="background:lime;">1 + 1;
</code></pre>
`````

````` {.r}
mark('````\nA _code chunk_:\n\n```{r, echo=TRUE}\n1 + 1;\n```\n````')
`````

````` {.html .plain}
<pre><code>A _code chunk_:

```{r, echo=TRUE}
1 + 1;
```
</code></pre>
`````

````` {.r}
# raw blocks
mark('```{=html}\n<p>raw HTML</p>\n```')
`````

````` {.html .plain}
<p>raw HTML</p>
`````

````` {.r}
mark('```{=latex}\n\\textbf{raw LaTeX}\n```')
`````

````` {.html .plain}

`````

````` {.r}
# fenced Divs
mark('::: foo\nasdf\n:::')
`````

````` {.html .plain}
<div class="foo">
<p>asdf</p>
</div>
`````

````` {.r}
mark('::: {.foo .bar #baz style="color: red;"}\nasdf\n:::')
`````

````` {.html .plain}
<div id="baz" class="foo bar" style="color: red;">
<p>asdf</p>
</div>
`````

````` {.r}
# smartypants example
mark('1/2 (c)')
`````

````` {.html .plain}
<p>1/2 (c)</p>
`````

````` {.r}
mark('1/2 (c)', options = '+smartypants')
`````

````` {.html .plain}
<p>½ ©</p>
`````

````` {.r}
mkd <- paste(names(litedown:::pants), collapse = ' ')
mark(mkd, options = '+smartypants')
`````

````` {.html .plain}
<p>½ ⅓ ⅔ ¼ ¾ ⅕ ⅖ ⅗ ⅘ ⅙ ⅚ ⅛ ⅜ ⅝ ⅞ ⅐ ⅑ ⅒ © ® ™</p>
`````

````` {.r}
# filter HTML tags
mkd = '<style>a {}</style><script type="text/javascript">console.log("No!");</script>\n[Hello](#)'
mark(mkd)
`````

````` {.html .plain}
<style>a {}</style><script type="text/javascript">console.log("No!");</script>
<p><a href="#">Hello</a></p>
`````

````` {.r}
mark(mkd, options = 'tagfilter')
`````

````` {.html .plain}
&lt;style>a {}&lt;/style>&lt;script type="text/javascript">console.log("No!");&lt;/script>
<p><a href="#">Hello</a></p>
`````

## HTML output of above examples

````` {.r}
# toc example
mkd <- c('# Header 1', 'p1', '## Header 2', 'p2')

mark(mkd, options = '+number_sections')
`````
<h1 id="chp:header-1"><span class="section-number main-number">1</span> Header 1</h1>
<p>p1</p>
<h2 id="sec:header-2"><span class="section-number">1.1</span> Header 2</h2>
<p>p2</p>

````` {.r}
mark(mkd, options = '+number_sections+toc')
`````
<div id="TOC">
<ul class="numbered">
<li><a href="#chp:header-1"><span class="section-number main-number">1</span> Header 1</a>
<ul>
<li><a href="#sec:header-2"><span class="section-number">1.1</span> Header 2</a></li>
</ul>
</li>
</ul>
</div>
<h1 id="chp:header-1"><span class="section-number main-number">1</span> Header 1</h1>
<p>p1</p>
<h2 id="sec:header-2"><span class="section-number">1.1</span> Header 2</h2>
<p>p2</p>

````` {.r}
# hard_wrap example
mark('foo\nbar\n')
`````
<p>foo
bar</p>

````` {.r}
mark('foo\nbar\n', options = '+hardbreaks')
`````
<p>foo<br />
bar</p>

````` {.r}
# latex math example
mkd <- c(
  '`$x$` is inline math $x$!', '', 'Display style:', '', '$$x + y$$', '',
  '\\begin{align}
a^{2}+b^{2} & = c^{2}\\\\
\\sin^{2}(x)+\\cos^{2}(x) & = 1
\\end{align}'
)

mark(mkd)
`````
<p><code>$x$</code> is inline math \(x\)!</p>
<p>Display style:</p>
<p>$$x + y$$</p>
<p>\begin{align}
a^{2}+b^{2} &amp; = c^{2}\\
\sin^{2}(x)+\cos^{2}(x) &amp; = 1
\end{align}</p>

````` {.r}
mark(mkd, options = '-latex_math')
`````
<p><code>$x$</code> is inline math $x$!</p>
<p>Display style:</p>
<p>$$x + y$$</p>
<p>\begin{align}
a^{2}+b^{2} &amp; = c^{2}\
\sin^{2}(x)+\cos^{2}(x) &amp; = 1
\end{align}</p>

````` {.r}
# table example
mark('
First Header  | Second Header
------------- | -------------
Content Cell  | Content Cell
Content Cell  | Content Cell
')
`````
<table>
<thead>
<tr>
<th>First Header</th>
<th>Second Header</th>
</tr>
</thead>
<tbody>
<tr>
<td>Content Cell</td>
<td>Content Cell</td>
</tr>
<tr>
<td>Content Cell</td>
<td>Content Cell</td>
</tr>
</tbody>
</table>

````` {.r}
# caption
mark('
| a | b |
|---|--:|
| A | 9 |

Table: A table _caption_.
')
`````
<table>
<caption>A table <em>caption</em>.</caption>
<thead>
<tr>
<th>a</th>
<th align="right">b</th>
</tr>
</thead>
<tbody>
<tr>
<td>A</td>
<td align="right">9</td>
</tr>
</tbody>
</table>

````` {.r}
# no table
mark('
First Header  | Second Header
------------- | -------------
Content Cell  | Content Cell
Content Cell  | Content Cell
', options = '-table')
`````
<p>First Header  | Second Header
———–– | ———––
Content Cell  | Content Cell
Content Cell  | Content Cell</p>

````` {.r}
# autolink example
mark('https://www.r-project.org/')
`````
<p><a href="https://www.r-project.org/">https://www.r-project.org/</a></p>

````` {.r}
mark('https://www.r-project.org/', options = '-autolink')
`````
<p>https://www.r-project.org/</p>

````` {.r}
# links and spans
mark('[a b](#){.red}')
`````
<p><a href="#" class="red">a b</a></p>

````` {.r}
mark('[a\nb](){.red}')
`````
<p><span class="red">a
b</span></p>

````` {.r}
# strikethrough example
mark('~~awesome~~')
`````
<p><del>awesome</del></p>

````` {.r}
mark('~~awesome~~', options = '-strikethrough')
`````
<p>~~awesome~~</p>

````` {.r}
# superscript and subscript examples
mark('2^10^')
`````
<p>2<sup>10</sup></p>

````` {.r}
mark('2^10^', options = '-superscript')
`````
<p>2^10^</p>

````` {.r}
mark('H~2~O')
`````
<p>H<sub>2</sub>O</p>

````` {.r}
mark('H~2~O', options = '-subscript')
`````
<p>H~2~O</p>

````` {.r}
# code blocks
mark('```r\n1 + 1;\n```')
`````
<pre><code class="language-r">1 + 1;
</code></pre>

````` {.r}
mark('```{.r}\n1 + 1;\n```')
`````
<pre><code class="language-r">1 + 1;
</code></pre>

````` {.r}
mark('```{.r .js}\n1 + 1;\n```')
`````
<pre><code class="language-r js">1 + 1;
</code></pre>

````` {.r}
mark('```{.r .js #foo}\n1 + 1;\n```')
`````
<pre><code class="language-r js" id="foo">1 + 1;
</code></pre>

````` {.r}
mark('```{.r .js #foo style="background:lime;"}\n1 + 1;\n```')
`````
<pre><code class="language-r js" id="foo" style="background:lime;">1 + 1;
</code></pre>

````` {.r}
mark('````\nA _code chunk_:\n\n```{r, echo=TRUE}\n1 + 1;\n```\n````')
`````
<pre><code>A _code chunk_:

```{r, echo=TRUE}
1 + 1;
```
</code></pre>

````` {.r}
# raw blocks
mark('```{=html}\n<p>raw HTML</p>\n```')
`````
<p>raw HTML</p>

````` {.r}
mark('```{=latex}\n\\textbf{raw LaTeX}\n```')
`````


````` {.r}
# fenced Divs
mark('::: foo\nasdf\n:::')
`````
<div class="foo">
<p>asdf</p>
</div>

````` {.r}
mark('::: {.foo .bar #baz style="color: red;"}\nasdf\n:::')
`````
<div id="baz" class="foo bar" style="color: red;">
<p>asdf</p>
</div>

````` {.r}
# smartypants example
mark('1/2 (c)')
`````
<p>1/2 (c)</p>

````` {.r}
mark('1/2 (c)', options = '+smartypants')
`````
<p>½ ©</p>

````` {.r}
mkd <- paste(names(litedown:::pants), collapse = ' ')
mark(mkd, options = '+smartypants')
`````
<p>½ ⅓ ⅔ ¼ ¾ ⅕ ⅖ ⅗ ⅘ ⅙ ⅚ ⅛ ⅜ ⅝ ⅞ ⅐ ⅑ ⅒ © ® ™</p>

````` {.r}
# filter HTML tags
mkd = '<style>a {}</style><script type="text/javascript">console.log("No!");</script>\n[Hello](#)'
mark(mkd)
`````
<style>a {}</style><script type="text/javascript">console.log("No!");</script>
<p><a href="#">Hello</a></p>

````` {.r}
mark(mkd, options = 'tagfilter')
`````
&lt;style>a {}&lt;/style>&lt;script type="text/javascript">console.log("No!");&lt;/script>
<p><a href="#">Hello</a></p>

## LaTeX output of above examples

````` {.r}
# temporarily override mark()
mark = function(...) litedown::mark(..., output = 'latex')
# toc example
mkd <- c('# Header 1', 'p1', '## Header 2', 'p2')

mark(mkd, options = '+number_sections')
`````

````` {.latex .plain}
\section{Header 1}

p1

\subsection{Header 2}

p2
`````

````` {.r}
mark(mkd, options = '+number_sections+toc')
`````

````` {.latex .plain}
\tableofcontents
\section{Header 1}

p1

\subsection{Header 2}

p2
`````

````` {.r}
# hard_wrap example
mark('foo\nbar\n')
`````

````` {.latex .plain}
foo
bar
`````

````` {.r}
mark('foo\nbar\n', options = '+hardbreaks')
`````

````` {.latex .plain}
foo\\
bar
`````

````` {.r}
# latex math example
mkd <- c(
  '`$x$` is inline math $x$!', '', 'Display style:', '', '$$x + y$$', '',
  '\\begin{align}
a^{2}+b^{2} & = c^{2}\\\\
\\sin^{2}(x)+\\cos^{2}(x) & = 1
\\end{align}'
)

mark(mkd)
`````

````` {.latex .plain}
\texttt{\$x\$} is inline math \(x\)!

Display style:

$$x + y$$

\begin{align}
a^{2}+b^{2} & = c^{2}\\
\sin^{2}(x)+\cos^{2}(x) & = 1
\end{align}
`````

````` {.r}
mark(mkd, options = '-latex_math')
`````

````` {.latex .plain}
\texttt{\$x\$} is inline math \$x\$!

Display style:

\$\$x + y\$\$

\textbackslash{}begin\{align\}
a\^{}\{2\}+b\^{}\{2\} \& = c\^{}\{2\}\textbackslash{}
\textbackslash{}sin\^{}\{2\}(x)+\textbackslash{}cos\^{}\{2\}(x) \& = 1
\textbackslash{}end\{align\}
`````

````` {.r}
# table example
mark('
First Header  | Second Header
------------- | -------------
Content Cell  | Content Cell
Content Cell  | Content Cell
')
`````

````` {.latex .plain}
\begin{tabular}{ll}
First Header & Second Header \\
Content Cell & Content Cell \\
Content Cell & Content Cell \\
\end{tabular}

`````

````` {.r}
# caption
mark('
| a | b |
|---|--:|
| A | 9 |

Table: A table _caption_.
')
`````

````` {.latex .plain}
\begin{tabular}{lr}
a & b \\
A & 9 \\
\end{tabular}

Table: A table \emph{caption}.
`````

````` {.r}
# no table
mark('
First Header  | Second Header
------------- | -------------
Content Cell  | Content Cell
Content Cell  | Content Cell
', options = '-table')
`````

````` {.latex .plain}
First Header  \textbar{} Second Header
------------- \textbar{} -------------
Content Cell  \textbar{} Content Cell
Content Cell  \textbar{} Content Cell
`````

````` {.r}
# autolink example
mark('https://www.r-project.org/')
`````

````` {.latex .plain}
\url{https://www.r-project.org/}
`````

````` {.r}
mark('https://www.r-project.org/', options = '-autolink')
`````

````` {.latex .plain}
https://www.r-project.org/
`````

````` {.r}
# links and spans
mark('[a b](#){.red}')
`````

````` {.latex .plain}
\protect\hyperlink{}{a b}\{.red\}
`````

````` {.r}
mark('[a\nb](){.red}')
`````

````` {.latex .plain}
{a
b}\{.red\}
`````

````` {.r}
# strikethrough example
mark('~~awesome~~')
`````

````` {.latex .plain}
\sout{awesome}
`````

````` {.r}
mark('~~awesome~~', options = '-strikethrough')
`````

````` {.latex .plain}
\textasciitilde{}\textasciitilde{}awesome\textasciitilde{}\textasciitilde{}
`````

````` {.r}
# superscript and subscript examples
mark('2^10^')
`````

````` {.latex .plain}
2\textsuperscript{10}
`````

````` {.r}
mark('2^10^', options = '-superscript')
`````

````` {.latex .plain}
2\^{}10\^{}
`````

````` {.r}
mark('H~2~O')
`````

````` {.latex .plain}
H\textsubscript{2}O
`````

````` {.r}
mark('H~2~O', options = '-subscript')
`````

````` {.latex .plain}
H\textasciitilde{}2\textasciitilde{}O
`````

````` {.r}
# code blocks
mark('```r\n1 + 1;\n```')
`````

````` {.latex .plain}
\begin{verbatim}
1 + 1;
\end{verbatim}
`````

````` {.r}
mark('```{.r}\n1 + 1;\n```')
`````

````` {.latex .plain}
\begin{verbatim}
1 + 1;
\end{verbatim}
`````

````` {.r}
mark('```{.r .js}\n1 + 1;\n```')
`````

````` {.latex .plain}
\begin{verbatim}
1 + 1;
\end{verbatim}
`````

````` {.r}
mark('```{.r .js #foo}\n1 + 1;\n```')
`````

````` {.latex .plain}
\begin{verbatim}
1 + 1;
\end{verbatim}
`````

````` {.r}
mark('```{.r .js #foo style="background:lime;"}\n1 + 1;\n```')
`````

````` {.latex .plain}
\begin{verbatim}
1 + 1;
\end{verbatim}
`````

````` {.r}
mark('````\nA _code chunk_:\n\n```{r, echo=TRUE}\n1 + 1;\n```\n````')
`````

````` {.latex .plain}
\begin{verbatim}
A _code chunk_:

```{r, echo=TRUE}
1 + 1;
```
\end{verbatim}
`````

````` {.r}
# raw blocks
mark('```{=html}\n<p>raw HTML</p>\n```')
`````

````` {.latex .plain}

`````

````` {.r}
mark('```{=latex}\n\\textbf{raw LaTeX}\n```')
`````

````` {.latex .plain}
\textbf{raw LaTeX}
`````

````` {.r}
# fenced Divs
mark('::: foo\nasdf\n:::')
`````

````` {.latex .plain}
\begin{verbatim}
::: {.foo} :::
\end{verbatim}

asdf
\end
`````

````` {.r}
mark('::: {.foo .bar #baz style="color: red;"}\nasdf\n:::')
`````

````` {.latex .plain}
\begin{verbatim}
::: {.foo .bar #baz style="color: red;"} :::
\end{verbatim}

asdf
\end
`````

````` {.r}
# smartypants example
mark('1/2 (c)')
`````

````` {.latex .plain}
1/2 (c)
`````

````` {.r}
mark('1/2 (c)', options = '+smartypants')
`````

````` {.latex .plain}
½ ©
`````

````` {.r}
mkd <- paste(names(litedown:::pants), collapse = ' ')
mark(mkd, options = '+smartypants')
`````

````` {.latex .plain}
½ ⅓ ⅔ ¼ ¾ ⅕ ⅖ ⅗ ⅘ ⅙ ⅚ ⅛ ⅜ ⅝ ⅞ ⅐ ⅑ ⅒ © ® ™
`````

````` {.r}
# filter HTML tags
mkd = '<style>a {}</style><script type="text/javascript">console.log("No!");</script>\n[Hello](#)'
mark(mkd)
`````

````` {.latex .plain}
\protect\hyperlink{}{Hello}
`````

````` {.r}
mark(mkd, options = 'tagfilter')
`````

````` {.latex .plain}
\protect\hyperlink{}{Hello}
`````

````` {.r}
rm(mark)
`````
