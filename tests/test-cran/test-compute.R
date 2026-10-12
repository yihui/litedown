library(testit)

fuse_md = function(text, ...) {
  as.character(fuse(text = text, output = 'markdown', quiet = TRUE, ...))
}

# a mock executor that records its arguments, then delegates to do.call()
new_recorder = function() {
  calls = list()
  ex = function(fun, args, profile) {
    calls[[length(calls) + 1L]] <<- list(fun = fun, args = args, profile = profile)
    do.call(fun, args)
  }
  list(ex = ex, calls = function() calls)
}

assert('compute: true forwards profile = NULL; compute: gpu forwards the profile name', {
  rec = new_recorder()
  options(compute.executor = rec$ex)
  fuse_md("```{r}\n#| compute: true\n1 + 1\n```\n")
  fuse_md("```{r}\n#| compute: gpu\n1 + 1\n```\n")
  options(compute.executor = NULL)
  calls = rec$calls()
  (length(calls) == 2L)
  (is.null(calls[[1L]]$profile))
  (calls[[2L]]$profile == 'gpu')
})

assert('untagged chunks do not reach the executor', {
  rec = new_recorder()
  options(compute.executor = rec$ex)
  out = fuse_md("```{r}\n1 + 1\n```\n")
  options(compute.executor = NULL)
  (length(rec$calls()) == 0L)
  (any(grepl('2', out)))
})

assert('cache: true on a routed chunk actually caches', {
  rec = new_recorder()
  options(compute.executor = rec$ex)
  # count evaluations via an environment variable: xfun::record() hashes the
  # values of globals referenced by the chunk, so a variable counter changes
  # its own cache hash
  Sys.setenv(LD_CACHE_COUNTER = '0')
  src = paste(
    "```{r}", "#| compute: true", "#| cache: true", "#| cache.path: 'ld-test-cache/'",
    "Sys.setenv(LD_CACHE_COUNTER = as.integer(Sys.getenv('LD_CACHE_COUNTER')) + 1L)",
    "```", sep = "\n"
  )
  invisible(fuse_md(src))
  invisible(fuse_md(src))
  n = as.integer(Sys.getenv('LD_CACHE_COUNTER'))
  Sys.unsetenv('LD_CACHE_COUNTER')
  options(compute.executor = NULL)
  unlink('ld-test-cache', recursive = TRUE)
  # evaluated only once: the second render used the cache
  (n == 1L)
})

assert('a routed chunk with both a plot and cache renders twice', {
  options(compute.executor = function(fun, args, profile) do.call(fun, args))
  src = paste(
    "```{r}", "#| compute: true", "#| cache: true", "#| cache.path: 'ld-test-cache/'",
    "#| fig.path: 'ld-test-fig-'", "plot(1:10)", "```", sep = "\n"
  )
  invisible(fuse_md(src))
  # the second render hits the worker's cache: the plot files from the first
  # run no longer exist there, and the host reuses the files it wrote then
  out = fuse_md(src)
  figs = Sys.glob('ld-test-fig-*.png')
  options(compute.executor = NULL)
  unlink('ld-test-cache', recursive = TRUE)
  unlink(figs)
  (length(figs) > 0L)
  (any(grepl('ld-test-fig-', out, fixed = TRUE)))
})

assert('an invalid compute value warns and evaluates locally', {
  rec = new_recorder()
  options(compute.executor = rec$ex)
  w = NULL
  out = withCallingHandlers(
    fuse_md("```{r}\n#| compute: 123\n1 + 1\n```\n"),
    warning = function(x) {
      w <<- conditionMessage(x)
      invokeRestart('muffleWarning')
    }
  )
  options(compute.executor = NULL)
  (grepl("invalid 'compute' chunk option", w, fixed = TRUE))
  (length(rec$calls()) == 0L)
  (any(grepl('2', out)))
})

assert('a tagged chunk with no registered executor errors informatively', {
  options(compute.executor = NULL)
  e = tryCatch(fuse_md("```{r}\n#| compute: true\n1 + 1\n```\n"), error = identity)
  (inherits(e, 'error'))
  (grepl('requires a registered executor', e$message, fixed = TRUE))
})

assert('an executor that raises propagates as the chunk error', {
  options(compute.executor = function(fun, args, profile) stop('backend exploded'))
  e = tryCatch(fuse_md("```{r}\n#| compute: true\n1 + 1\n```\n"), error = identity)
  options(compute.executor = NULL)
  (inherits(e, 'error'))
  (grepl('backend exploded', e$message, fixed = TRUE))
})
