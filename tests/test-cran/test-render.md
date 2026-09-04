# Snapshot tests for the C renderers and GFM/litedown extensions

The renderers (`html.c`, `latex.c`, `plaintext.c`, `man.c`, `xml.c`) and the
GFM/litedown extensions (`table.c`, `autolink.c`, `tagfilter.c`,
`strikethrough.c`, `tasklist.c`) are only reachable through these entry points,
so their exact output is pinned here.

## GFM table to HTML (with column alignment)

```r
cat(markdown_html(c('|a|b|', '|:-|-:|', '|1|2|'), extensions = 'table'))
```
```
<table>
<thead>
<tr>
<th align="left">a</th>
<th align="right">b</th>
</tr>
</thead>
<tbody>
<tr>
<td align="left">1</td>
<td align="right">2</td>
</tr>
</tbody>
</table>
```

## GFM table to LaTeX tabular

```r
cat(markdown_latex(c('|a|b|', '|:-|-:|', '|1|2|'), extensions = 'table'))
```
```
\begin{table}
\begin{tabular}{lr}
a & b \\
1 & 2 \\
\end{tabular}
\end{table}
```

## Autolink: URLs, www, and emails

```r
cat(markdown_html('see https://x.com, www.x.com, and me@x.com', extensions = 'autolink'))
```
```
<p>see <a href="https://x.com">https://x.com</a>, <a href="http://www.x.com">www.x.com</a>, and <a href="mailto:me@x.com">me@x.com</a></p>
```

## Tagfilter neutralizes dangerous raw HTML tags

```r
cat(markdown_html(c('<script>alert(1)</script>', '', '<b>ok</b>'), extensions = 'tagfilter'))
```
```
&lt;script>alert(1)&lt;/script>
<p><b>ok</b></p>
```

## Strikethrough to HTML and LaTeX

```r
cat(markdown_html('~~x~~', extensions = 'strikethrough'))
cat(markdown_latex('~~x~~', extensions = 'strikethrough'))
```
```
<p><del>x</del></p>
\sout{x}
```

## Task list checkboxes

```r
cat(markdown_html(c('- [x] done', '- [ ] todo'), extensions = 'tasklist'))
```
```
<ul>
<li><input type="checkbox" checked="" /> done</li>
<li><input type="checkbox" /> todo</li>
</ul>
```

## LaTeX: sectioning, lists, quote, code, and rule

```r
cat(markdown_latex(c(
  '# h1', '', '## h2', '', '1. a', '2. b', '', '- x', '- y', '',
  '> quote', '', '```', 'code', '```', '', '---'
)))
```
```
\section{h1}

\subsection{h2}

\begin{enumerate}
\item a

\item b

\end{enumerate}

\begin{itemize}
\item x

\item y

\end{itemize}

\begin{quote}
quote

\end{quote}

\begin{verbatim}
code
\end{verbatim}

\begin{center}\rule{0.5\linewidth}{\linethickness}\end{center}
```

## LaTeX escapes special characters

```r
cat(markdown_latex('100% a_b #c $d & e'))
```
```
100\% a\_b \#c \$d \& e
```

## LaTeX: emphasis, strong, code, link, image

```r
cat(markdown_latex('*a* **b** `c` [t](http://u) ![alt](i.png)'))
```
```
\emph{a} \textbf{b} \texttt{c} \href{http://u}{t} \protect\includegraphics{i.png}
```

## LaTeX: ordered list not starting at 1 sets the counter

```r
cat(markdown_latex(c('4. x', '5. y')))
```
```
\begin{enumerate}
\setcounter{enumi}{4}
\item x

\item y

\end{enumerate}
```

## Plain-text renderer strips inline markup

```r
cat(markdown_text('# Hi *world* and `code`'))
```
```
Hi world and code
```

## Man (roff) renderer

```r
cat(markdown_man(c('# Title', '', 'a *em* and **strong**', '', '- one', '- two')))
```
```
.SH
Title
.PP
a \f[I]em\f[] and \f[B]strong\f[]
.IP \[bu] 2
one
.IP \[bu] 2
two
```

## XML renderer emits a CommonMark document tree

```r
cat(markdown_xml('# Hi [t](u)'))
```
```
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE document SYSTEM "CommonMark.dtd">
<document xmlns="http://commonmark.org/xml/1.0">
  <heading level="1">
    <text xml:space="preserve">Hi </text>
    <link destination="u" title="">
      <text xml:space="preserve">t</text>
    </link>
  </heading>
</document>
```

## Commonmark renderer normalizes Markdown

```r
cat(markdown_commonmark(c('#   Hi', '', '_a_ and **b**')))
```
```
# Hi

*a* and **b**
```

## HTML escapes metacharacters in text

```r
cat(markdown_html('a < b & c > d'))
```
```
<p>a &lt; b &amp; c &gt; d</p>
```
