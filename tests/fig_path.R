local({
    opts = options("litedown.html.options" = list("embed_resources" = FALSE))
    on.exit(options(opts), add = TRUE)

    react = litedown::reactor(fig.path = "foo")
    on.exit(litedown::reactor(react), add = TRUE)

    text = litedown::fuse(text = "```{r}\nplot(1)\n```", output = "markdown")
    link = strsplit(text, "\n")[[1]][4]
    stopifnot(identical("![](<foochunk-1-1.png>)", link))
    unlink("foochunk-1-1.png")

    litedown::reactor(fig.path = "foo/bar")
    text = litedown::fuse(text = "```{r}\nplot(1)\n```", output = "markdown")
    link = strsplit(text, "\n")[[1]][4]
    stopifnot(identical("![](<foo/barchunk-1-1.png>)", link))
    unlink("foo", recursive = TRUE)
})
