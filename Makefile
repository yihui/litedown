all:
	ln -f ../lite.js/js/snap.js ../lite.js/css/snap.css ../lite.js/css/default.css inst/resources/
	Rscript -e "Rd2roxygen::rab('.', install=TRUE)"
	rm litedown_*.tar.gz
	cd examples && Rscript _run.R
