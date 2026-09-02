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

assert('mark() toggles superscript/subscript/strikethrough independently', {
  # each option controls only its own feature, even though ~ drives two of them
  (mark2('~~x~~', 'html', options = '-strikethrough') %==% '<p>~~x~~</p>')
  (mark2('~x~', 'html', options = '-strikethrough') %==% '<p><sub>x</sub></p>')
  (mark2('~x~', 'html', options = '-subscript') %==% '<p>~x~</p>')
  (mark2('~~x~~', 'html', options = '-subscript') %==% '<p><del>x</del></p>')
  (mark2('x^2^', 'html', options = '-superscript') %==% '<p>x^2^</p>')
})
