library(testit)

# find the first node of a given type via a depth-first walk
find_node = function(node, type) {
  if (identical(node$type, type)) return(node)
  for (child in node$children) {
    if (!is.null(res <- find_node(child, type))) return(res)
  }
  NULL
}

assert('markdown_ast() returns a document node with children', {
  x = markdown_ast('# Hi\n\ntext\n')
  (x$type %==% 'document')
  (length(x$children) %==% 2L)
  (x$children[[1]]$type %==% 'heading')
  (x$children[[1]]$level %==% 1L)
})

assert('markdown_ast() reports code block info string and literal', {
  x = markdown_ast('```r\n1 + 1\n```\n')
  cb = find_node(x, 'code_block')
  (cb$info %==% 'r')
  (cb$literal %==% '1 + 1\n')
})

# regression test for the inline sourcepos column fix: inline code on an
# indented continuation line must report the correct source column
assert('inline sourcepos is correct on indented continuation lines', {
  x = markdown_ast('A paragraph that wraps\n   with `code` here.\n')
  code = find_node(x, 'code')
  # `code` sits on line 2; the backtick is at column 9, so the code content
  # (without fences) starts at column 10 and ends at column 13
  (code$sourcepos %==% c(2L, 10L, 2L, 13L))
})

assert('inline sourcepos is correct inside an indented blockquote', {
  x = markdown_ast('> line one\n>    with `code` here\n')
  code = find_node(x, 'code')
  (code$sourcepos %==% c(2L, 12L, 2L, 15L))
})

assert('markdown_code_tokens() collects code blocks and inline code', {
  d = markdown_code_tokens(c('`{r} x` and `y`', '', '```{r}', '1', '```'))
  (d$type %==% c('code', 'code', 'code_block'))
  (d$literal[1:2] %==% c('{r} x', 'y'))
  (d$info %==% c(NA_character_, NA_character_, '{r}'))
  # the fix above must also hold on the path crack() actually uses
  d2 = markdown_code_tokens('A paragraph that wraps\n   with `code` here.\n')
  (c(d2$start_line, d2$start_col, d2$end_line, d2$end_col) %==% c(2L, 10L, 2L, 13L))
})

assert('markdown_html() still renders correctly', {
  (markdown_html('Hello _World_!\n') %==% '<p>Hello <em>World</em>!</p>\n')
})

# the 'attributes' extension renders Pandoc-style code block attributes
# (```{.class #id key="val"}) as HTML attributes on <pre><code>, replacing the
# id_string()/convert_attrs() smuggling that mark() used to do in R
assert('the attributes extension renders code block attributes', {
  cb = function(x) markdown_html(x, extensions = 'attributes')
  # a single class becomes the language- class (same as a plain info string)
  (cb('```{.r}\n1\n```\n') %==% '<pre><code class="language-r">1\n</code></pre>\n')
  # multiple classes are space-joined; #id and key="val" follow class, in order
  (cb('```{.r .js #foo style="color: red;"}\n1\n```\n') %==% paste0(
    '<pre><code class="language-r js" id="foo" style="color: red;">1\n',
    '</code></pre>\n'
  ))
  # {-} is shorthand for .unnumbered
  (cb('```{-}\n1\n```\n') %==% '<pre><code class="language-unnumbered">1\n</code></pre>\n')
  # an id with no class emits no class attribute (no empty language- class)
  (cb('```{#foo}\n1\n```\n') %==% '<pre><code id="foo">1\n</code></pre>\n')
  # class/id values are HTML-escaped (matching cmark's language class escaping)
  (cb('```{.a<b #c>d}\n1\n```\n') %==%
     '<pre><code class="language-a&lt;b" id="c&gt;d">1\n</code></pre>\n')
  # key=value attributes are emitted verbatim (as the old R path did; litedown
  # runs cmark in UNSAFE mode and treats the input as trusted)
  (cb('```{style="width: 50%"}\n1\n```\n') %==%
     '<pre><code style="width: 50%">1\n</code></pre>\n')
  # a raw content block ({=...}) is left to the rawblock extension, not claimed
  (markdown_html('```{=html}\n<hr>\n```\n', extensions = c('rawblock', 'attributes')) %==%
     '<hr>\n')
})

# litedown patches the tasklist extension to render checkboxes without the
# disabled="" attribute (upstream cmark-gfm emits it), so they are interactive;
# this replaces a gsub() workaround that used to strip disabled="" in mark()
assert('task list checkboxes are rendered without disabled=""', {
  (markdown_html('- [x] a\n- [ ] b\n', extensions = 'tasklist') %==% paste0(
    '<ul>\n',
    '<li><input type="checkbox" checked="" /> a</li>\n',
    '<li><input type="checkbox" /> b</li>\n',
    '</ul>\n'
  ))
})

# a bare closing tag opens a type 6/7 HTML block that, per CommonMark, ends only
# at a blank line; litedown patches cmark so an opening code fence ends it too,
# and the fence is parsed as a code block instead of being absorbed as HTML
assert('an opening code fence ends a preceding HTML block', {
  (markdown_html('</p>\n```\nx\n```\n') %==%
     '</p>\n<pre><code>x\n</code></pre>\n')
  # a tilde fence works the same way
  (markdown_html('</p>\n~~~\nx\n~~~\n') %==%
     '</p>\n<pre><code>x\n</code></pre>\n')
  # an HTML block not followed by a fence is still a single block
  (markdown_html('<div>\nhi\n</div>\nmore\n') %==% '<div>\nhi\n</div>\nmore\n')
})

assert('prose_index() returns the lines that are not inside a code block', {
  # plain prose: every line
  (prose_index(c('a', 'b', 'c')) %==% 1:3)
  # fenced code block: the fence and its body are excluded
  (prose_index(c('a', '```', 'x', '```', 'b')) %==% c(1L, 5L))
  (prose_index(c('a', '```r', 'x', '```', 'b')) %==% c(1L, 5L))
  # a tilde fence is a code fence too
  (prose_index(c('a', '~~~', '```', '~~~', 'b')) %==% c(1L, 5L))
  # indented (4-space) code blocks are excluded (xfun::prose_index missed these);
  # cmark includes the trailing blank line in the block, so line 4 is code too
  (prose_index(c('a', '', '    code', '', 'b')) %==% c(1L, 2L, 5L))
  # an unclosed fence runs to the end of the document (no all-prose fallback)
  (prose_index(c('a', '```', 'x')) %==% 1L)
  # raw HTML blocks remain prose (only code blocks are excluded)
  (prose_index(c('a', '<pre>', 'x', '</pre>', 'b')) %==% 1:5)
  # edge cases
  (prose_index(character(0)) %==% integer(0))
  (prose_index(c('```', 'x', '```')) %==% integer(0))
  # an element carrying an embedded newline still maps to a single index
  (prose_index(c('a\nb', '```', 'x', '```')) %==% 1L)
})

assert('GFM extensions are inert unless explicitly enabled', {
  # exact renderer output for enabled extensions is pinned in test-render.md;
  # here we only check that the features stay off by default
  (!grepl('<table', markdown_html('|a|b|\n|-|-|\n|1|2|\n')))
  (!grepl('<del>', markdown_html('~~x~~\n')))
  (!grepl('<a href', markdown_html('see https://x.com\n')))
})
