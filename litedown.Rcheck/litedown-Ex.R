pkgname <- "litedown"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('litedown')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("crack")
### * crack

flush(stderr()); flush(stdout())

### Name: crack
### Title: Parse R Markdown or R scripts
### Aliases: crack sieve

### ** Examples

library(litedown)
# parse R Markdown
res = crack(c("```{r}\n1+1\n```", "Hello, `pi` = `{r} pi` and `e` = `{r} exp(1)`!"))
str(res)
# evaluate inline code and combine results with text fragments
txt = lapply(res[[2]]$source, function(x) {
    if (is.character(x))
        x else eval(parse(text = x$source))
})
paste(unlist(txt), collapse = "")

# parse R code
res = sieve(c("#' This is _doc_.", "", "#| eval=TRUE", "# this is code", "1 + 1"))
str(res)



cleanEx()
nameEx("engines")
### * engines

flush(stderr()); flush(stdout())

### Name: engines
### Title: Language engines
### Aliases: engines

### ** Examples

litedown::engines()  # built-in engines



cleanEx()
nameEx("get_context")
### * get_context

flush(stderr()); flush(stdout())

### Name: get_context
### Title: Get the 'fuse()' context
### Aliases: get_context

### ** Examples

litedown::get_context("input")
litedown::get_context("format")
names(litedown::get_context())  # all available items



cleanEx()
nameEx("mark")
### * mark

flush(stderr()); flush(stdout())

### Name: fuse
### Title: Render Markdown, R Markdown, and R scripts
### Aliases: fuse fiss mark

### ** Examples

library(litedown)
doc = c("```{r}", "1 + 1", "```", "", "$\\pi$ = `{r} pi`.")
fuse(doc)
fuse(doc, ".tex")
fiss(doc)

mark(c("Hello _World_!", "", "Welcome to **litedown**."))
# if input appears to be a file path but should be treated as text, use I()
mark(I("This is *not* a file.md"))
# that's equivalent to
mark(text = "This is *not* a file.md")

# output to a file
(mark("_Hello_, **World**!", output = tempfile()))

# convert to other formats
mark("Hello _World_!", ".tex")
mark("Hello _**`World`**_!", "xml")
mark("Hello _**`World`**_!", "text")



cleanEx()
nameEx("markdown_options")
### * markdown_options

flush(stderr()); flush(stdout())

### Name: markdown_options
### Title: Markdown rendering options
### Aliases: markdown_options

### ** Examples

# all available options
litedown::markdown_options()



cleanEx()
nameEx("pkg_desc")
### * pkg_desc

flush(stderr()); flush(stdout())

### Name: pkg_desc
### Title: Print the package description, news, citation, manual pages, and
###   source code
### Aliases: pkg_desc pkg_news pkg_code pkg_citation pkg_manual

### ** Examples

## Not run: 
##D litedown::pkg_desc()
##D litedown::pkg_news()
##D litedown::pkg_citation()
## End(Not run)



cleanEx()
nameEx("raw_text")
### * raw_text

flush(stderr()); flush(stdout())

### Name: raw_text
### Title: Mark a character vector as raw output
### Aliases: raw_text

### ** Examples

litedown::raw_text(c("**This**", "_is_", "[Markdown](#)."))
litedown::raw_text("<b>Bold</b>", "html")
litedown::raw_text("\\textbf{Bold}", "latex")



cleanEx()
nameEx("reactor")
### * reactor

flush(stderr()); flush(stdout())

### Name: reactor
### Title: Get and set chunk options
### Aliases: reactor

### ** Examples

# get options
litedown::reactor("echo")
litedown::reactor(c("echo", "fig.width"))

# set options
old = litedown::reactor(echo = FALSE, fig.width = 8)
litedown::reactor(c("echo", "fig.width"))
litedown::reactor(old)  # restore options

# use the environment directly
opts = litedown::reactor()
opts$echo
mget(c("echo", "fig.width"), opts)
ls(opts)  # built-in options



cleanEx()
nameEx("vest")
### * vest

flush(stderr()); flush(stdout())

### Name: vest
### Title: Add CSS/JS assets to HTML output
### Aliases: vest

### ** Examples

litedown:::assets[, -1]
# add features
litedown::vest(c("copy-button", "tabsets"))
# add css/js directly
litedown::vest(css = "@tabsets", js = c("@tabsets", "@fold-details"))



### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
