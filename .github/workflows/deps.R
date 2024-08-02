if (getRversion() <= '3.1.3') for (m in c('wget', 'curl')) if (Sys.which(m) != '') {
  options(download.file.method = m);
  cat(sprintf('\noptions(download.file.method = "%s")\n', m), file = '~/.Rprofile', append = TRUE)
  break
}
install.packages(c('commonmark', 'rbibutils'), repos = 'https://cloud.r-project.org', INSTALL_opts = '--no-help')
system2('git', c('clone', '--depth=1', 'https://github.com/yihui/xfun'))
system2('R', c('CMD', 'INSTALL', 'xfun'))
unlink('xfun', recursive = TRUE)
