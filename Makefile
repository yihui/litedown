all:
	ln -f ../lite.js/js/snap.js ../lite.js/css/snap.css ../lite.js/css/default.css inst/resources/
	Rscript -e 'roxygen2::roxygenize(".")'
	rm litedown_*.tar.gz
	cd examples && Rscript _run.R
