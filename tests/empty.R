library(litedown)
f = tempfile()
if (file.create(f)) {
  # create full HTML
  opts = options(litedown.html.template = TRUE)
  mark(f)
  options(opts)

  # create fragment
  mark(f)
  unlink(f)
}
