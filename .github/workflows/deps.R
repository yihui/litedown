if (getRversion() <= '3.1.3') for (m in c('wget', 'curl')) if (Sys.which(m) != '') {
  options(download.file.method = m); break
}
install.packages(c("commonmark", "rbibutils"), repos = "https://cloud.r-project.org", INSTALL_opts = "--no-help")
install.packages("xfun", repos = "https://yihui.r-universe.dev")
