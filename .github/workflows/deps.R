if (getRversion() <= '3.2.1') for (m in c('wget', 'curl')) if (Sys.which(m) != '') {
  options(download.file.method = m)
  cat(sprintf('\noptions(download.file.method = "%s")\n', m), file = '~/.Rprofile', append = TRUE)
  break
}
install.packages(c('commonmark', 'xfun', 'rbibutils'), INSTALL_opts = '--no-help', quiet = TRUE)
