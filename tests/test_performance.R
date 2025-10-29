# Simple performance tests to verify optimizations work correctly

# Test 1: merge_list
result1 = litedown:::merge_list(list(a = 1, b = 2), list(b = 3, c = 4))
stopifnot(identical(result1, list(a = 1, b = 3, c = 4)))

# Test 2: id_string
text = c("Hello world", "This is test text", "No special strings here")
id1 = litedown:::id_string(text)
stopifnot(is.character(id1) && length(id1) == 1)
# Verify the ID is not present in any of the text elements
stopifnot(!any(vapply(text, function(t) grepl(id1, t, fixed = TRUE), logical(1))))

# Test 3: resolve_dups
x = c('@foo', 'foo', '@bar', 'baz')
result3 = litedown:::resolve_dups(x)
# Should keep @foo and remove foo, keep @bar, keep baz
stopifnot('@foo' %in% result3)
stopifnot(!('foo' %in% result3))
stopifnot('@bar' %in% result3)
stopifnot('baz' %in% result3)

# Test 4: Basic rendering still works
md = "# Hello\n\nThis is **bold** and *italic*."
result4 = litedown::mark(text = md, output = NA)
stopifnot(grepl("<h1", result4))
stopifnot(grepl("<strong>bold</strong>", result4))
stopifnot(grepl("<em>italic</em>", result4))
