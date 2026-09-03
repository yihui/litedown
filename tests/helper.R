# Test helper: returns "" if pattern matches x, or x unchanged otherwise.
# Use (matches(x, ".*foo.*") %==% "") to assert presence, or
# (matches(x, ".*foo.*") %==% x) to assert absence; both show x on failure.
matches = function(x, pattern) {
  x = paste(x, collapse = "\n")
  sub(paste0("(?s)", pattern), "", x, perl = TRUE)
}
