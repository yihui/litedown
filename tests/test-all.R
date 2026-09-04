library(testit)
test_pkg('litedown', 'test-cran')
# CI-only tests (e.g. HTML output of examples) across operating systems
if (tolower(Sys.getenv('CI')) == 'true') test_pkg('litedown', 'test-ci')
