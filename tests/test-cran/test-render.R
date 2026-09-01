library(testit)

ns = asNamespace('litedown')
markdown_ast = ns$markdown_ast
markdown_html = ns$markdown_html

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

assert('markdown_html() still renders correctly', {
  (markdown_html('Hello _World_!\n') %==% '<p>Hello <em>World</em>!</p>\n')
})
