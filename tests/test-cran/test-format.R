library(testit)

assert('normalize_yaml() renames rmarkdown output formats to litedown formats', {
  # a bare string output format with no options
  (normalize_yaml(list(output = 'html_document'))$output %==% list(html = list()))
  # pdf_document -> latex, html_vignette -> html
  (names(normalize_yaml(list(output = 'pdf_document'))$output) %==% 'latex')
  (names(normalize_yaml(list(output = 'html_vignette'))$output) %==% 'html')
  # the `format` key is normalized to `output`
  y = normalize_yaml(list(format = 'html'))
  (is.null(y$format))
  (names(y$output) %==% 'html')
  # litedown::/markdown:: prefixes and _format suffixes are stripped
  (names(normalize_yaml(list(output = 'litedown::html_format'))$output) %==% 'html')
  (names(normalize_yaml(list(output = 'markdown::latex_format'))$output) %==% 'latex')
  # no output/format field -> NULL (nothing to normalize)
  (is.null(normalize_yaml(list(title = 'x'))))
})

assert('normalize_yaml() maps rmarkdown output options through map_args()', {
  y = normalize_yaml(list(output = list(pdf_document = list(toc = TRUE))))
  o = y$output$latex
  (o$options$toc$depth %==% 3)
  (o$meta$css %==% 'default')
})

assert('normalize_yaml() errors on a non-list, non-character output field', {
  (has_error(normalize_yaml(list(output = 1:3))))
})

assert('detect_format() detects the format from the output name or extension', {
  # a known format name is returned as-is
  (detect_format('html', NULL) %==% 'html')
  (detect_format('latex', NULL) %==% 'latex')
  # .pdf implies latex
  (detect_format('.pdf', NULL) %==% 'latex')
  (detect_format('out.pdf', NULL) %==% 'latex')
  # a file extension maps to its format
  (detect_format('foo.html', NULL) %==% 'html')
  (detect_format('foo.tex', NULL) %==% 'latex')
  (detect_format('foo.md', NULL) %==% 'markdown')
  # markdown:xxx reports the final format (markdown is the intermediate step)
  (detect_format('markdown:latex', NULL) %==% 'latex')
})

assert('detect_format() falls back to the YAML output format', {
  # a non-character output falls through to yaml_format()
  (detect_format(NULL, list(output = list(latex = list()))) %==% 'latex')
  # unknown extension with no YAML defaults to html
  (detect_format(NULL, list()) %==% 'html')
})

assert('yaml_format() returns the first output format or defaults to html', {
  (yaml_format(list(output = list(latex = list(), html = list()))) %==% 'latex')
  (yaml_format(list(output = 'text')) %==% 'text')
  # no output field -> html
  (yaml_format(list()) %==% 'html')
})

assert('yaml_field() extracts a named field under an output format', {
  y = list(output = list(html = list(meta = list(css = 'x'), options = list(toc = TRUE))))
  (yaml_field(y, 'html', 'meta') %==% list(css = 'x'))
  (yaml_field(y, 'html', 'options') %==% list(toc = TRUE))
  # a length>1 name selects a sublist
  (names(yaml_field(y, 'html', c('meta', 'options'))) %==% c('meta', 'options'))
  # missing format -> NULL
  (is.null(yaml_field(y, 'latex', 'meta')))
})

assert('map_args() maps rmarkdown html_document arguments to litedown options', {
  a = map_args(toc = TRUE, toc_depth = 2, number_sections = TRUE)
  (a$options$toc$depth %==% 2)
  (isTRUE(a$options$number_sections))
  (a$meta$css %==% 'default')
  # self_contained maps to embed_resources
  (isFALSE(map_args(self_contained = FALSE)$options$embed_resources))
})

assert('map_args() adds assets for mathjax, anchors, and code folding', {
  a = map_args(math_method = 'mathjax', anchor_sections = TRUE, code_folding = 'show')
  (a$options$js_math %==% 'mathjax')
  ('@heading-anchor' %in% a$meta$js)
  ('@heading-anchor' %in% a$meta$css)
  ('@fold-details' %in% a$meta$js)
  # math_method as a list uses its $engine
  (map_args(math_method = list(engine = 'mathjax'))$options$js_math %==% 'mathjax')
  # extra css is appended after 'default'
  (map_args(css = 'custom.css')$meta$css %==% c('default', 'custom.css'))
})

assert('map_args() maps rmarkdown includes to litedown meta fields', {
  a = map_args(includes = list(in_header = 'h', before_body = 'b', after_body = 'a'))
  (a$meta$header_includes %==% 'h')
  (a$meta$include_before %==% 'b')
  (a$meta$include_after %==% 'a')
})
