library(testit)

assert('reactor() can get multiple options at once', {
  res = reactor(c('echo', 'eval'))
  (is.list(res))
  (length(res) %==% 2L)
  (names(res) %==% c('echo', 'eval'))
})

assert('reactor() can set options and returns old values', {
  old = reactor(echo = FALSE, fig.width = 5)
  (isTRUE(old$echo))
  (old$fig.width %==% 7)
  (isFALSE(reactor('echo')))
  (reactor('fig.width') %==% 5)
  # restore
  reactor(old)
  (isTRUE(reactor('echo')))
  (reactor('fig.width') %==% 7)
})

assert('reactor() accepts a list as first unnamed argument to set options', {
  old = reactor(list(echo = FALSE, fig.width = 3))
  (isFALSE(reactor('echo')))
  (reactor('fig.width') %==% 3)
  reactor(old)
  (isTRUE(reactor('echo')))
  (reactor('fig.width') %==% 7)
})

assert('reactor() allows accessing options via $ on returned environment', {
  opts = reactor()
  (isTRUE(opts$echo))
  (isTRUE(opts$eval))
})

assert('engines() can register and retrieve a custom engine', {
  old = engines(mytest = function(x, inline = FALSE, ...) 'custom')
  (is.function(engines('mytest')))
  (engines('mytest')(NULL) %==% 'custom')
  # restore
  engines(old)
  (is.null(engines('mytest')))
})

assert('fuse_env() returns the global environment when called outside fuse()', {
  (fuse_env() %==% globalenv())
})

assert('get_context() returns NULL for items outside fuse()', {
  (is.null(get_context('input')))
  (is.null(get_context('format')))
})

