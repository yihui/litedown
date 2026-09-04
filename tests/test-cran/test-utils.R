library(testit)

assert('smartypants() transforms ASCII typographic markers', {
  # all pants names produce their HTML entities
  mkd = paste(names(pants), collapse = ' ')
  (smartypants(mkd) %==% paste(
    '&frac12; &frac13; &frac23; &frac14; &frac34; &frac15; &frac25; &frac35;',
    '&frac45; &frac16; &frac56; &frac18; &frac38; &frac58; &frac78;',
    '&#8528; &#8529; &#8530; &copy; &reg; &trade;'
  ))
  # fractions are transformed
  (smartypants('1/2') %==% '&frac12;')
  (smartypants('1/3') %==% '&frac13;')
  (smartypants('3/4') %==% '&frac34;')
  # copyright/trademark symbols
  (smartypants('(c)') %==% '&copy;')
  (smartypants('(r)') %==% '&reg;')
  (smartypants('(tm)') %==% '&trade;')
  # no transformation in code blocks
  (smartypants(c('text (c)', '```', '(c)', '```')) %==% c('text &copy;', '```', '(c)', '```'))
})

assert('merge_list() merges named lists, later values override earlier ones', {
  (merge_list(list(a = 1), list(b = 2)) %==% list(a = 1, b = 2))
  (merge_list(list(a = 1), list(a = 2)) %==% list(a = 2))
  (merge_list(list(a = 1, b = 2), list(a = 3)) %==% list(a = 3, b = 2))
  # non-list arguments are ignored
  (merge_list(list(a = 1), NULL) %==% list(a = 1))
  (merge_list(list(a = 1), 'not a list') %==% list(a = 1))
  # first list wins when later args are non-list
  (merge_list(list(a = 1), list(), list(a = 2)) %==% list(a = 2))
})

assert('is_file() detects whether an input looks like a file path', {
  # a string with an extension looks like a file
  (is_file('foo.md'))
  (is_file('path/to/file.Rmd'))
  # I() wrapping disables file detection
  (!is_file(I('foo.md')))
  # a character vector of length > 1 is not a file
  (!is_file(c('a.md', 'b.md')))
  # a string with no extension and non-existent path is not a file
  (!is_file('justtext'))
  # a plain string that is a bare word (no ext, not existing) is not a file
  (!is_file('hello world'))
})

assert('restore_html() unescapes HTML entities', {
  (restore_html('&quot;') %==% '"')
  (restore_html('&amp;') %==% '&')
  (restore_html('&lt;') %==% '<')
  (restore_html('&gt;') %==% '>')
  (restore_html('a &lt;b&gt; c') %==% 'a <b> c')
  # multiple entities in one string
  (restore_html('&lt;div&gt;&amp;&lt;/div&gt;') %==% '<div>&</div>')
})

assert('locale_lang() returns a BCP 47 language tag string', {
  lang = locale_lang()
  (is.character(lang))
  # either empty or a valid BCP 47 tag (letters, possibly with a hyphen and region)
  (nchar(lang) == 0L || grepl('^[a-z]{2,3}(-[A-Z]{2,3})?$', lang))
})

assert('sans_p() strips a wrapping <p> tag and trailing newline', {
  (sans_p('<p>hi</p>\n') %==% 'hi')
  # a <p> with attributes is stripped too
  (sans_p('<p class="x">a</p>\n') %==% 'a')
  # an opening <p> with no closing tag still loses the tag and newline
  (sans_p('<p>abc\n') %==% 'abc')
})

assert("sans_sq() removes single quotes around words", {
  (sans_sq("use 'R' and 'knitr' here") %==% 'use R and knitr here')
  # quotes not surrounding a bare word are left alone
  (sans_sq("it's fine") %==% "it's fine")
})

assert('str_trim() and trim_blank() trim whitespace', {
  (str_trim('  hi  ') %==% 'hi')
  (str_trim('\tx\n') %==% 'x')
  # trim_blank removes blank lines from both ends but keeps inner content
  (trim_blank('\n\n  x\n  \n') %==% '  x')
})

assert('comma_list() quotes and comma-joins its input', {
  (comma_list(c('a', 'b', 'c')) %==% '"a", "b", "c"')
  (comma_list('a') %==% '"a"')
})

assert('one_string() collapses a vector with a separator', {
  (one_string(c('a', 'b')) %==% 'a\nb')          # default newline
  (one_string(c('a', 'b'), ' ') %==% 'a b')
})

assert('is_ext() detects a bare file extension', {
  (is_ext('.md'))
  (is_ext('.html'))
  (!is_ext('foo.md'))   # has a base name
  (!is_ext('md'))       # no leading dot
})

assert('has_class() detects a class token in an HTML attribute string', {
  (has_class('<div class="a b c">', 'b'))
  (has_class('<div class="a">', 'a'))
  (!has_class('<div class="a">', 'b'))
  # a partial match must not count (foobar is not the class foo)
  (!has_class('<div class="foobar">', 'foo'))
})

assert('unique_id() disambiguates duplicated ids and fills empties', {
  (unique_id(c('a', 'a', 'b'), 'sec') %==% c('a_1', 'a_2', 'b'))
  # empty strings are replaced with the fallback first
  (unique_id(c('', 'x'), 'sec') %==% c('sec', 'x'))
  (unique_id(c('', ''), 'sec') %==% c('sec_1', 'sec_2'))
})

assert('merge_list() overrides earlier values and appends new keys', {
  (merge_list(list(a = 1, b = 2), list(b = 3, c = 4)) %==% list(a = 1, b = 3, c = 4))
})

assert('na_omit() and dropNULL() drop missing/NULL elements', {
  (na_omit(c(1, NA, 3)) %==% c(1, 3))
  (dropNULL(list(a = 1, b = NULL, c = 3)) %==% list(a = 1, c = 3))
})

assert('set_names() assigns names to a vector', {
  (set_names(1:2, c('x', 'y')) %==% c(x = 1L, y = 2L))
})

assert('named_bool() builds a named logical list', {
  (named_bool(c('a', 'b')) %==% list(a = TRUE, b = TRUE))
  (named_bool('a', FALSE) %==% list(a = FALSE))
})

assert('redefine_level() raises LaTeX sectioning levels', {
  (redefine_level('\\section{x}', 'chapter') %==% '\\chapter{x}')
  (redefine_level('\\section{x}', 'part') %==% '\\part{x}')
  (redefine_level('\\subsection{x}', 'chapter') %==% '\\section{x}')
  # top = 'section' (or anything but chapter/part) is a no-op
  (redefine_level('\\section{x}', 'section') %==% '\\section{x}')
})

assert('latex_envir() turns a nesting spec into begin/end pairs', {
  # {A}, '', ... closes the most recent environment on an empty entry
  (latex_envir(c('{A}', '')) %==% c('\\begin{A}', '\\end{A}'))
  (latex_envir(c('{A}', '{B}', '', '')) %==%
     c('\\begin{A}', '\\begin{B}', '\\end{B}', '\\end{A}'))
})

assert('sub_var() substitutes the first matching variable name in a template', {
  (sub_var('a $x$ b', '$x$', 'Y') %==% 'a Y b')
  # the first name present wins; a missing earlier name is skipped
  (sub_var('a NAME b', c('MISSING', 'NAME'), 'Z') %==% 'a Z b')
  # no match leaves the template unchanged
  (sub_var('nothing here', 'ABSENT', 'Z') %==% 'nothing here')
})

assert('match_full() returns whole matches split by line', {
  (match_full('a1 b2 c3', '[a-z][0-9]') %==% c('a1', 'b2', 'c3'))
})

assert('match_all() returns a matrix of full match plus capture groups', {
  m = match_all('k1=v1 k2=v2', '([a-z0-9]+)=([a-z0-9]+)')
  (m[1, ] %==% c('k1=v1', 'k2=v2'))
  (m[2, ] %==% c('k1', 'k2'))
  (m[3, ] %==% c('v1', 'v2'))
})

assert('match_one() captures groups from a single match', {
  (match_one('k=v', '([a-z]+)=([a-z]+)')[[1]] %==% c('k=v', 'k', 'v'))
})

assert('is_output_file() recognizes a real output path (not a format/ext)', {
  (is_output_file('foo.html'))
  (!is_output_file('html'))   # a known format name
  (!is_output_file('.md'))    # a bare extension
})

assert('is_output_full() reads the full attribute', {
  (is_output_full(structure('x', full = TRUE)))
  (!is_output_full('x'))
})

assert('auto_output() derives an output name from format or extension', {
  # NULL output + a format -> the format's default extension (named by format)
  (unname(auto_output(I('txt'), NULL, 'html')) %==% '.html')
  # an unsupported format errors
  (has_error(auto_output(I('x'), NULL, 'bogus')))
  # a bare extension + a file input -> input path with that extension
  (auto_output('in.Rmd', '.html') %==% 'in.html')
})

assert('auto_identifier() adds ids to headings and keeps existing ones', {
  (auto_identifier('<h2>Hello World</h2>') %==% '<h2 id="sec:hello-world">Hello World</h2>')
  # h1 gets a chp: prefix; h2+ get sec:
  (auto_identifier('<h1>Intro</h1>') %==% '<h1 id="chp:intro">Intro</h1>')
  # a heading that already has an id is left untouched
  (auto_identifier('<h2 id="x">Y</h2>') %==% '<h2 id="x">Y</h2>')
})

assert('build_toc() builds a nested TOC from HTML headings', {
  toc = build_toc(c('<h1 id="a">A</h1>', '<h2 id="b">B</h2>'))
  (grepl('id="TOC"', toc, fixed = TRUE))
  (grepl('<a href="#a">A</a>', toc, fixed = TRUE))
  (grepl('<a href="#b">B</a>', toc, fixed = TRUE))
  # headings marked class="unlisted" are excluded, dropping the TOC below 2 items
  (is.null(build_toc(c('<h1 id="a">A</h1>', '<h2 id="b" class="unlisted">B</h2>'))))
  # n <= 0 disables the TOC
  (is.null(build_toc(c('<h1 id="a">A</h1>', '<h2 id="b">B</h2>'), 0)))
})

assert('number_sections() prefixes hierarchical numbers to HTML headings', {
  x = number_sections(c('<h1>A</h1>', '<h2>B</h2>', '<h1>C</h1>'))
  (grepl('main-number">1</span> A', x, fixed = TRUE))
  (grepl('section-number">1.1</span> B', x, fixed = TRUE))
  (grepl('main-number">2</span> C', x, fixed = TRUE))
  # input with no headings is returned unchanged
  (number_sections('<p>no heading</p>') %==% '<p>no heading</p>')
})
