library(testit)

assert('reactor() can get multiple options at once', {
  res = litedown::reactor(c('echo', 'eval'))
  (is.list(res))
  (length(res) %==% 2L)
  (names(res) %==% c('echo', 'eval'))
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

