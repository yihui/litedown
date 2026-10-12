all:
	ln -f ../lite.js/js/snap.js ../lite.js/css/snap.css ../lite.js/css/default.css inst/resources/
	Rscript -e 'roxygen2::roxygenize(".")'
	R CMD INSTALL litedown_*.tar.gz
	rm litedown_*.tar.gz
	cd examples && Rscript _run.R
