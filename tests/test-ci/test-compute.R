library(testit)

# end-to-end contract test against a real mirai backend; sync daemons evaluate
# in-process (mirai >= 2.5.1), so no daemon processes, ports or sleeps
if (requireNamespace('mirai', quietly = TRUE)) {

  # a mirai-backed executor. Per the executor contract it raises on error
  # rather than returning error-classed objects
  ex = function(fun, args, profile) {
    res = mirai::mirai(do.call(fun, args), .args = list(fun = fun, args = args), .compute = profile)[]
    if (mirai::is_mirai_error(res)) stop(res)
    res
  }
  mirai::daemons(sync = TRUE)
  options(compute.executor = ex)

  assert('a tagged chunk evaluates via the mirai executor', {
    out = fuse(text = "```{r}\n#| compute: true\n1 + 1\n```\n", output = 'markdown', quiet = TRUE)
    (any(grepl('2', out)))
  })

  assert('a figure from a routed chunk lands in fig.path', {
    src = "```{r}\n#| compute: true\n#| fig.path: 'ld-fig-'\nplot(1:10)\n```\n"
    out = fuse(text = src, output = 'markdown', quiet = TRUE)
    files = Sys.glob('ld-fig-chunk-1-*.png')
    unlink(files)
    (length(files) > 0L)
  })

  assert('untagged chunks still evaluate host-side in fuse_env()', {
    e = new.env()
    e$host_var = 42L
    out = fuse(text = "```{r}\nhost_var\n```\n", output = 'markdown', quiet = TRUE, envir = e)
    (any(grepl('42', out)))
  })

  options(compute.executor = NULL)
  mirai::daemons(0)

}
