library(testit)

assert('reactor() returns an environment with no arguments', {
  opts = litedown::reactor()
  (is.environment(opts))
  (inherits(opts, 'litedown_env'))
})

assert('reactor() has sensible default values', {
  (isTRUE(litedown::reactor('eval')))
  (isTRUE(litedown::reactor('echo')))
  (isTRUE(litedown::reactor('warning')))
  (isTRUE(litedown::reactor('message')))
  (is.na(litedown::reactor('error')))
  (isTRUE(litedown::reactor('include')))
  (isTRUE(litedown::reactor('strip.white')))
  (isFALSE(litedown::reactor('collapse')))
  (litedown::reactor('fig.width') %==% 7)
  (litedown::reactor('fig.height') %==% 7)
  (litedown::reactor('signif') %==% 3)
  (litedown::reactor('power') %==% 6)
})

assert('reactor() can get multiple options at once', {
  res = litedown::reactor(c('echo', 'eval'))
  (is.list(res))
  (length(res) %==% 2L)
  (identical(names(res), c('echo', 'eval')))
})

assert('reactor() can set options and returns old values', {
  old = litedown::reactor(echo = FALSE, fig.width = 5)
  (isTRUE(old$echo))
  (old$fig.width %==% 7)
  (isFALSE(litedown::reactor('echo')))
  (litedown::reactor('fig.width') %==% 5)
  # restore
  litedown::reactor(old)
  (isTRUE(litedown::reactor('echo')))
  (litedown::reactor('fig.width') %==% 7)
})

assert('reactor() accepts a list as first unnamed argument to set options', {
  old = litedown::reactor(list(echo = FALSE, fig.width = 3))
  (isFALSE(litedown::reactor('echo')))
  (litedown::reactor('fig.width') %==% 3)
  litedown::reactor(old)
  (isTRUE(litedown::reactor('echo')))
  (litedown::reactor('fig.width') %==% 7)
})

assert('reactor() allows accessing options via $ on returned environment', {
  opts = litedown::reactor()
  (isTRUE(opts$echo))
  (isTRUE(opts$eval))
})

assert('engines() returns an environment with no arguments', {
  eng = litedown::engines()
  (is.environment(eng))
})

assert('engines() has built-in engines registered', {
  (is.function(litedown::engines('r')))
  (is.function(litedown::engines('md')))
  (is.function(litedown::engines('css')))
  (is.function(litedown::engines('js')))
})

assert('engines() can register and retrieve a custom engine', {
  old = litedown::engines(mytest = function(x, inline = FALSE, ...) 'custom')
  (is.function(litedown::engines('mytest')))
  (litedown::engines('mytest')(NULL) %==% 'custom')
  # restore
  litedown::engines(old)
  (is.null(litedown::engines('mytest')))
})

assert('fuse_env() returns the global environment when called outside fuse()', {
  (identical(litedown::fuse_env(), globalenv()))
})

assert('get_context() returns NULL for items outside fuse()', {
  (is.null(litedown::get_context('input')))
  (is.null(litedown::get_context('format')))
})

assert('get_context() returns an environment with no argument', {
  ctx = litedown::get_context()
  (is.environment(ctx))
})
