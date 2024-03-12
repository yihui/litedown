library(litedown)
f = tempfile()
if (file.create(f)) {
   mark_html(f, template = FALSE)
   mark_html(f)
   unlink(f)
}
