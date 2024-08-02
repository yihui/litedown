# for R versions < 4.0

# ignore the perl argument for regexec() if it's not supported (in R < 3.3); we
# use perl = TRUE only for speed in this package
if (!'perl' %in% names(formals(regexec)))
  regexec = function(..., perl) base::regexec(...)

has_fun = function(name, envir = baseenv()) exists(name, envir, inherits = FALSE)

if (!has_fun('isFALSE')) isFALSE = function(x) {
  is.logical(x) && length(x) == 1 && !is.na(x) && !x
}
if (!has_fun('startsWith')) startsWith = function(x, s) {
  substring(x, 1, nchar(s)) == s
}
if (!has_fun('endsWith')) endsWith = function(x, s) {
  n = nchar(x)
  substring(x, n - nchar(s) + 1, n) == s
}
if (!has_fun('anyNA')) anyNA = function(x) any(is.na(x))
if (!has_fun('dir.exists')) dir.exists = function(x) xfun::dir_exists(x)
if (!has_fun('strrep')) strrep = function(x, times) {
  mapply(function(x, n) paste(rep(x, n), collapse = ''), x, times, USE.NAMES = FALSE)
}
