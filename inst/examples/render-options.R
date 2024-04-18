library(litedown)

# toc example
mkd <- c("# Header 1", "p1", "## Header 2", "p2")

cat(mark(mkd, options = "+number_sections"))
cat(mark(mkd, options = "+number_sections+toc"))

# hard_wrap example
cat(mark("foo\nbar\n"))
cat(mark("foo\nbar\n", options = "+hardbreaks"))

# latex math example
mkd <- c(
  "`$x$` is inline math $x$!", "", "Display style:", "", "$$x + y$$", "",
  "\\begin{eqnarray}
a^{2}+b^{2} & = & c^{2}\\\\
\\sin^{2}(x)+\\cos^{2}(x) & = & 1
\\end{eqnarray}"
)

cat(mark(mkd))
cat(mark(mkd, options = "-latex_math"))

# tables example (need 4 spaces at beginning of line here)
cat(mark("
First Header  | Second Header
------------- | -------------
Content Cell  | Content Cell
Content Cell  | Content Cell
"))

# but not here
cat(mark("
First Header  | Second Header
------------- | -------------
Content Cell  | Content Cell
Content Cell  | Content Cell
", options = '-table'))

# autolink example
cat(mark("https://www.r-project.org/"))
cat(mark("https://www.r-project.org/", options = "-autolink"))

# strikethrough example
cat(mark("~~awesome~~"))
cat(mark("~~awesome~~", options = "-strikethrough"))

# superscript and subscript examples
cat(mark("2^10^"))
cat(mark("2^10^", options = "-superscript"))
cat(mark("H~2~O"))
cat(mark("H~2~O", options = "-subscript"))

# code blocks
cat(mark('```r\n1 + 1;\n```'))
cat(mark('```{.r}\n1 + 1;\n```'))
cat(mark('```{.r .js}\n1 + 1;\n```'))
cat(mark('```{.r .js #foo}\n1 + 1;\n```'))
cat(mark('```{.r .js #foo style="color:red;"}\n1 + 1;\n```'))
cat(mark('````\n```{r, echo=TRUE}\n1 + 1;\n```\n````'))

# raw blocks
cat(mark('```{=html}\n<p>raw HTML</p>\n```'))
cat(mark('```{=latex}\n<p>raw HTML</p>\n```'))

# skip_html tags
mkd = '<style>a {}</style><script type="text/javascript">console.log("No!");</script>\n[Hello](#)'
cat(mark(mkd))
# TODO: wait for https://github.com/r-lib/commonmark/issues/15 to be fixed
# cat(mark(mkd, options = "tagfilter"))
