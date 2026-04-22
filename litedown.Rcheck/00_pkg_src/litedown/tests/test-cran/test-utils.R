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

assert('markdown_options() returns a character vector of all available options', {
  opts = litedown::markdown_options()
  (is.character(opts))
  (length(opts) > 10L)
  # options enabled by default have a + prefix
  (any(startsWith(opts, '+')))
  # options disabled by default have a - prefix
  (any(startsWith(opts, '-')))
  # result is sorted
  (identical(opts, sort(opts)))
  # known enabled options are present
  ('+embed_resources' %in% opts)
  ('+latex_math' %in% opts)
  ('+superscript' %in% opts)
  ('+subscript' %in% opts)
  # known disabled options are present
  ('-toc' %in% opts)
  ('-number_sections' %in% opts)
  ('-smartypants' %in% opts)
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
