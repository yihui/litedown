library(testit)

assert('code blocks after as-is HTML tags are rendered correctly', {
  (!grepl('```', litedown::fuse(text = '#| results="asis"\ncat("<p>hi</p>\n")\n#| foo\n1:2')))
})
