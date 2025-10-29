# Performance Improvements

This document describes performance optimizations made to the litedown package.

## Optimizations Implemented

### 1. Vectorized Pattern Matching in `embed_resources()` (utils.R:1016)

**Before:**
```r
for (i in grep('^data:.+;base64,.+', z2, invert = TRUE)) {
  # process each non-encoded image
}
```

**After:**
```r
not_encoded = !grepl('^data:.+;base64,.+', z2)
for (i in which(not_encoded)) {
  # process each non-encoded image
}
```

**Impact:** Reduced overhead from `grep()` call with `invert=TRUE` and improved readability.

### 2. Simplified Loop Logic in `merge_list()` (utils.R:85-95)

**Before:**
```r
for (i in seq_along(dots) - 1L) {
  if (i == 0) next
  x = dots[[i + 1]]
  if (!is.list(x)) next
  res[names(x)] = x
}
```

**After:**
```r
n = length(dots)
if (n == 0) return(list())
if (n == 1) return(dots[[1]])
res = dots[[1]]
for (i in 2:n) {
  x = dots[[i]]
  if (is.list(x)) res[names(x)] = x
}
```

**Impact:** 
- Eliminated arithmetic overhead from `seq_along(dots) - 1L`
- Removed conditional branch inside loop
- Added early returns for edge cases
- More readable and maintainable

### 3. Optimized String Search in `id_string()` (utils.R:101-110)

**Before:**
```r
for (i in lens) {
  for (j in seq_len(times)) {
    id = paste(sample(CHARS, i, replace = TRUE), collapse = '')
    if (length(grep(id, text, fixed = TRUE)) == 0) return(id)
  }
}
```

**After:**
```r
text_combined = paste(text, collapse = '\n')
for (i in lens) {
  for (j in seq_len(times)) {
    id = paste(sample(CHARS, i, replace = TRUE), collapse = '')
    if (!grepl(id, text_combined, fixed = TRUE)) return(id)
  }
}
```

**Impact:**
- Combines text vector once instead of multiple times in grep
- Uses `grepl()` instead of `length(grep(...)) == 0` (more idiomatic)
- Significant speedup when `text` is a long vector

### 4. Vectorized Duplicate Removal in `resolve_dups()` (utils.R:1201-1207)

**Before:**
```r
x = unique(x)
for (i in grep('^@', x, value = TRUE)) {
  x = x[x != sub('^@', '', i)]
}
```

**After:**
```r
x = unique(x)
has_at = startsWith(x, '@')
if (!any(has_at)) return(x)
at_items = x[has_at]
without_at = sub('^@', '', at_items)
# Keep items with @ or items that don't have an @ version
x[has_at | !(x %in% without_at)]
```

**Impact:**
- Eliminates loop with repeated vector operations
- Single vectorized operation instead of iterative subsetting
- More efficient memory usage
- Clearer logical expression

## Performance Testing

To test these improvements, you can run the following benchmark:

```r
library(litedown)

# Test 1: merge_list with multiple lists
bench_merge = function(n = 100) {
  lists = lapply(1:n, function(i) list(a = i, b = i * 2))
  system.time({
    for (i in 1:1000) {
      result = do.call(merge_list, lists)
    }
  })
}

# Test 2: id_string with large text
bench_id = function() {
  text = rep(paste(sample(letters, 100, replace = TRUE), collapse = ''), 1000)
  system.time({
    for (i in 1:100) {
      id = id_string(text)
    }
  })
}

# Test 3: resolve_dups with many items
bench_resolve = function() {
  x = c(paste0('@pkg', 1:100), paste0('pkg', 1:100))
  system.time({
    for (i in 1:10000) {
      result = litedown:::resolve_dups(x)
    }
  })
}

# Run benchmarks
print("merge_list benchmark:")
print(bench_merge())

print("id_string benchmark:")
print(bench_id())

print("resolve_dups benchmark:")
print(bench_resolve())
```

## Future Optimization Opportunities

### High Priority
1. **Caching compiled regex patterns** - Many regex patterns are used repeatedly
2. **Batch file operations** - Download/read operations could be batched better
3. **Memoization** - Cache results of expensive pure functions

### Medium Priority
4. **Parallel processing** - Some independent operations could run in parallel
5. **Lazy evaluation** - Delay expensive operations until actually needed
6. **String builder pattern** - For functions that build strings incrementally

### Low Priority
7. **Memory pre-allocation** - Some vectors could be pre-allocated
8. **Micro-optimizations** - Various small improvements in hot paths

## Benchmarking Results

Results will be added after running benchmarks on representative workloads.

## Notes

- All optimizations maintain backward compatibility
- No changes to function signatures or return values
- All existing tests should pass
- Code readability is preserved or improved
