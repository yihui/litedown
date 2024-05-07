library(litedown)

# toc example
mkd <- c("# Header 1", "p1", "## Header 2", "p2")

mark(mkd, options = "+number_sections")
mark(mkd, options = "+number_sections+toc")

# hard_wrap example
mark("foo\nbar\n")
mark("foo\nbar\n", options = "+hardbreaks")

# latex math example
mkd <- c(
  "`$x$` is inline math $x$!", "", "Display style:", "", "$$x + y$$", "",
  "\\begin{align}
a^{2}+b^{2} & = c^{2}\\\\
\\sin^{2}(x)+\\cos^{2}(x) & = 1
\\end{align}"
)

mark(mkd)
mark(mkd, options = "-latex_math")

# table example
mark("
First Header  | Second Header
------------- | -------------
Content Cell  | Content Cell
Content Cell  | Content Cell
")

# caption
mark("
| a | b |
|---|--:|
| A | 9 |

Table: A table _caption_.
")

# no table
mark("
First Header  | Second Header
------------- | -------------
Content Cell  | Content Cell
Content Cell  | Content Cell
", options = '-table')

# autolink example
mark("https://www.r-project.org/")
mark("https://www.r-project.org/", options = "-autolink")

# strikethrough example
mark("~~awesome~~")
mark("~~awesome~~", options = "-strikethrough")

# superscript and subscript examples
mark("2^10^")
mark("2^10^", options = "-superscript")
mark("H~2~O")
mark("H~2~O", options = "-subscript")

# code blocks
mark('```r\n1 + 1;\n```')
mark('```{.r}\n1 + 1;\n```')
mark('```{.r .js}\n1 + 1;\n```')
mark('```{.r .js #foo}\n1 + 1;\n```')
mark('```{.r .js #foo style="background:lime;"}\n1 + 1;\n```')
mark('````\nA _code chunk_:\n\n```{r, echo=TRUE}\n1 + 1;\n```\n````')

# raw blocks
mark('```{=html}\n<p>raw HTML</p>\n```')
mark('```{=latex}\n<p>raw HTML</p>\n```')

# filter out HTML tags
mkd = '<style>a {}</style><script type="text/javascript">console.log("No!");</script>\n[Hello](#)'
mark(mkd)
# tagfiler doesn't work: https://github.com/r-lib/commonmark/issues/15
# mark(mkd, options = "tagfilter")
