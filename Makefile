all:
	ln -f ../lite.js/js/snap.js ../lite.js/css/snap.css ../lite.js/css/default.css inst/resources/
	Rscript -e 'roxygen2::roxygenize(".")'
	./.githooks/pre-commit
	R CMD build .
	R CMD INSTALL *_*.tar.gz
	rm *_*.tar.gz
	cd examples && Rscript _run.R
