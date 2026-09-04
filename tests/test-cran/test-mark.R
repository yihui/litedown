library(testit)

# mark() returns a raw_string (with a 'lang' attribute); strip it for %==% checks
mark2 = function(...) as.character(mark(...))

assert('mark() with empty or trivial input produces empty output', {
  # character(0) gives a length-0 raw_string
  (length(mark(character(0))) %==% 0L)
  # empty string gives an empty raw_string
  (nchar(mark('')) %==% 0L)
  # empty file does not error and writes an empty HTML file
  f = tempfile()
  file.create(f)
  out = mark(f)
  (file_exists(out))
  (file.info(out)[, 'size'] %==% 0)
  unlink(c(f, out))
})

assert('mark() writes to a file when given a file path and returns it invisibly', {
  f = tempfile(fileext = '.html')
  ret = mark('Hello.', output = f)
  (file_exists(f))
  (ret %==% f)
  unlink(f)
})

assert('mark() treats I() input as text, not a file path', {
  out = mark(I('foo.md'), output = NA)
  (grepl('foo', out))
})

assert('mark() supports LaTeX and plain text output formats', {
  tex = mark('Hello _world_!', '.tex')
  (grepl('\\\\emph', tex))
  txt = mark('Hello _world_!', 'text')
  (grepl('Hello world', txt))
})

assert('mark() processes YAML metadata and applies title to output', {
  src = c('---', 'title: My Doc', '---', '', '# Hello')
  out = mark(I(src))
  (grepl('<title>My Doc</title>', out, fixed = TRUE))
})

assert('mark() renders inline and display LaTeX math via the C extension', {
  # inline $...$ -> \(...\); only &, <, > are escaped in HTML (not quotes)
  (mark2('a $x < y$ b', 'html') %==% '<p>a \\(x &lt; y\\) b</p>')
  # display $$...$$ is kept verbatim for the JS math engine to render
  (mark2(c('d:', '', '$$a & b$$'), 'html') %==% '<p>d:</p>\n<p>$$a &amp; b$$</p>')
  # LaTeX output keeps math bodies verbatim (no escaping)
  (mark2('a $x < y$ b', 'latex') %==% 'a \\(x < y\\) b')
})

assert('mark() does not treat currency or spaced dollars as math', {
  # $ followed by a digit, or padded with spaces, is not math (matches xfun rules)
  (mark2('price $5 and $6', 'html') %==% '<p>price $5 and $6</p>')
  (mark2('bad $ x $ here', 'html') %==% '<p>bad $ x $ here</p>')
  # math inside inline code is left untouched
  (grepl('<code>$x$</code>', mark('code `$x$` stays', 'html'), fixed = TRUE))
})

assert('mark() protects math bodies from Markdown interpretation (yihui/litedown#33)', {
  # underscores/asterisks/brackets in math must not become emphasis/links
  (mark2('$a_1 * b_2$', 'html') %==% '<p>\\(a_1 * b_2\\)</p>')
  (mark2('$f[x]$', 'html') %==% '<p>\\(f[x]\\)</p>')
})

assert('mark() renders LaTeX environments verbatim via the C extension', {
  env = c('\\begin{align}', 'a_1 & = b\\\\', 'c & = d', '\\end{align}')
  # HTML: wrapped in <p>, backslashes preserved (incl. \\), only &<> escaped
  h = mark(env, 'html')
  (grepl('<p>\\begin{align}', h, fixed = TRUE))
  (grepl('a_1 &amp; = b\\\\', h, fixed = TRUE))
  (grepl('\\end{align}</p>', h, fixed = TRUE))
  # LaTeX: emitted verbatim, no escaping of backslashes or &
  l = mark(env, 'latex')
  (grepl('\\begin{align}', l, fixed = TRUE))
  (grepl('a_1 & = b\\\\', l, fixed = TRUE))
})

assert('mark() disables math when the latex_math option is off', {
  (mark2('a $x$ b', 'html', options = '-latex_math') %==% '<p>a $x$ b</p>')
})

assert('mark() loads a math library only when math is actually rendered', {
  # render a full HTML document (with a template) and check whether KaTeX is
  # loaded; it should be loaded iff real math ends up in the output
  has_katex = function(x, options = NULL) {
    out = mark(I(c('---', 'title: t', '---', '', x)), NA, options = options)
    any(grepl('katex', out, ignore.case = TRUE))
  }
  # real math -> load
  (has_katex('a $x$ b'))
  (has_katex(c('d:', '', '$$x + y$$')))
  (has_katex(c('\\begin{align}', 'a &= b', '\\end{align}')))
  # false positives that must NOT load a math library
  (!has_katex('inline code `$x$` here'))       # $ inside inline code
  (!has_katex('it costs $5 and $6 total'))      # currency
  (!has_katex(c('```', 'f \\(x)', '```')))      # \( inside a code block
  (!has_katex(c('`$x$` $x$!', '', '$$x + y$$'), options = '-latex_math'))  # disabled
  (!has_katex('no math at all'))
})

assert('mark() renders superscript, subscript, and strikethrough via the C extension', {
  # ^x^ -> <sup>, ~x~ -> <sub>, ~~x~~ -> <del>
  (mark2('H~2~O and E=mc^2^', 'html') %==% '<p>H<sub>2</sub>O and E=mc<sup>2</sup></p>')
  (mark2('~~gone~~', 'html') %==% '<p><del>gone</del></p>')
  # content between delimiters is parsed as inline Markdown (nesting works)
  (mark2('a^b*c*^', 'html') %==% '<p>a<sup>b<em>c</em></sup></p>')
  # LaTeX output
  (mark2('H~2~O', 'latex') %==% 'H\\textsubscript{2}O')
  (mark2('E=mc^2^', 'latex') %==% 'E=mc\\textsuperscript{2}')
  (mark2('~~gone~~', 'latex') %==% '\\sout{gone}')
})

assert('mark() keeps [^x] as a footnote reference, not a superscript', {
  out = mark2(c('text[^1]', '', '[^1]: note.'), 'html')
  (grepl('<sup class="footnote-ref">', out, fixed = TRUE))
})

assert('mark() renders LaTeX footnotes inline via the C extension', {
  # the definition body is moved inline to the first reference as \footnote{}
  # (cmark otherwise moves definitions to the end of the document)
  (mark2(c('Hi[^a] there.', '', '[^a]: A _note_.'), 'latex') %==%
     'Hi\\footnote{A \\emph{note}.} there.')
  # two distinct footnotes each render as their own \footnote{}
  (mark2(c('A[^a] B[^b].', '', '[^a]: first.', '', '[^b]: second.'), 'latex') %==%
     'A\\footnote{first.} B\\footnote{second.}.')
  # a repeated reference reuses the number via \footnotemark[N] (N is the
  # footnote index), so LaTeX's footnote counter is not advanced twice
  (mark2(c('A[^a] again[^a].', '', '[^a]: shared.'), 'latex') %==%
     'A\\footnote{shared.} again\\footnotemark[1].')
  # footnotes disabled: no \footnote is emitted (the reference stays literal)
  (!grepl('\\footnote', mark2(c('A[^a].', '', '[^a]: x.'), 'latex', options = '-footnotes'),
          fixed = TRUE))
})

assert('mark() toggles superscript/subscript/strikethrough independently', {
  # each option controls only its own feature, even though ~ drives two of them
  (mark2('~~x~~', 'html', options = '-strikethrough') %==% '<p>~~x~~</p>')
  (mark2('~x~', 'html', options = '-strikethrough') %==% '<p><sub>x</sub></p>')
  (mark2('~x~', 'html', options = '-subscript') %==% '<p>~x~</p>')
  (mark2('~~x~~', 'html', options = '-subscript') %==% '<p><del>x</del></p>')
  (mark2('x^2^', 'html', options = '-superscript') %==% '<p>x^2^</p>')
})

assert('mark() renders raw content blocks via the C extension', {
  raw = function(lang, body) c(sprintf('```{=%s}', lang), body, '```')
  # ```{=html} -> verbatim in HTML output, discarded in LaTeX output
  (mark2(raw('html', '<hr class="x">'), 'html') %==% '<hr class="x">')
  (mark2(raw('html', '<hr>'), 'latex') %==% '')
  # ```{=latex} / ```{=tex} -> verbatim in LaTeX output, discarded in HTML output
  (mark2(raw('latex', '\\textbf{x}'), 'latex') %==% '\\textbf{x}')
  (mark2(raw('tex', '\\foo'), 'latex') %==% '\\foo')
  (mark2(raw('latex', '\\textbf{x}'), 'html') %==% '')
  # an unknown target (e.g. {=markdown}) is discarded in every format
  (mark2(raw('markdown', '*x*'), 'html') %==% '')
  (mark2(raw('markdown', '*x*'), 'latex') %==% '')
  # raw blocks don't escape their body and sit between surrounding prose
  (mark2(c('before', '', raw('html', '<hr>'), '', 'after'), 'html') %==%
     '<p>before</p>\n<hr>\n<p>after</p>')
})

assert('mark() applies {.class #id key=val} attributes in a canonical order', {
  # convert_attrs() (headings/links/divs) and the code block 'attributes'
  # extension share one C implementation, so all of them emit attributes in the
  # order class, id, then the remaining key=value tokens (in source order),
  # regardless of the order the tokens appear in the source
  (mark2('# Hi {.foo .bar #baz style="x"}') %==%
     '<h1 class="foo bar" id="baz" style="x">Hi</h1>')
  # tokens in a scrambled order still come out class, id, rest
  (mark2('# Hi {style="a" .foo k="b" #id}') %==%
     '<h1 class="foo" id="id" style="a" k="b">Hi</h1>')
  # a fenced Div and an inline link use the same ordering
  (mark2(c('::: {#baz .foo}', 'x', ':::')) %==%
     '<div class="foo" id="baz">\n<p>x</p>\n</div>')
  (mark2('[t](u){#baz .foo}') %==% '<p><a href="u" class="foo" id="baz">t</a></p>')
  # a code block goes through the same C core (with the language- class prefix)
  (mark2(c('```{#baz .foo}', 'x', '```')) %==%
     '<pre><code class="language-foo" id="baz">x\n</code></pre>')
})

assert('mark() attaches inline attributes to links, images, and spans in C', {
  # the 'inlineattrs' extension handles {...} after a link/image at the C level
  # image: class first, then remaining key=value tokens; width kept verbatim
  (mark2('![alt](a.png){.round width="400"}') %==%
     '<p><img src="a.png" alt="alt" class="round" width="400" /></p>')
  # an empty link href becomes a <span> carrying the attributes
  (mark2('[t](){.foo}') %==% '<p><span class="foo">t</span></p>')
  # a link with no trailing {...} is untouched
  (mark2('[t](u)') %==% '<p><a href="u">t</a></p>')
  # smart quotes inside the attribute list are normalized back to straight quotes
  (mark2('[t](u){k="v"}') %==% '<p><a href="u" k="v">t</a></p>')

  # LaTeX: image percent width -> \linewidth; other attributes dropped, and no
  # stray brace text is left behind on links/spans
  (mark2('![alt](a.png){width="40%" .foo}', 'latex') %==%
     '\\protect\\includegraphics[width=0.4\\linewidth]{a.png}')
  (mark2('[t](u){.foo}', 'latex') %==% '\\href{u}{t}')
  (mark2('[t](){.foo}', 'latex') %==% '{t}')
})

assert('mark() renders a raw LaTeX math environment as math in HTML too', {
  # documented exception: a raw {=latex}/{=tex} block that is a math environment
  # is wrapped in <p>...</p> for HTML so the JS math library can typeset it
  env = c('```{=latex}', '\\begin{align}', 'a &= b', '\\end{align}', '```')
  (mark2(env, 'html') %==% '<p>\n\\begin{align}\na &amp;= b\n\\end{align}\n</p>')
  # in LaTeX output it is emitted verbatim (no <p> wrapper, no escaping)
  (mark2(env, 'latex') %==% '\\begin{align}\na &= b\n\\end{align}')
  # it loads a math library even when latex_math is disabled, because the raw
  # block is rendered as math regardless of that option
  has_katex = function(x, options = NULL) {
    out = mark(I(c('---', 'title: t', '---', '', x)), NA, options = options)
    any(grepl('katex', out, ignore.case = TRUE))
  }
  (has_katex(env))
  (has_katex(env, options = '-latex_math'))
  # a raw HTML block or a non-math raw LaTeX block must NOT load a math library
  (!has_katex(c('```{=html}', '<hr>', '```')))
  (!has_katex(c('```{=latex}', '\\textbf{x}', '```')))
})
