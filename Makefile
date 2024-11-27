tests: tests/tests.Rout.save

tests/tests.Rout.save: tests/tests.R inst/examples/render-options.R
	ln -f ../lite.js/js/snap.js ../lite.js/css/snap.css ../lite.js/css/default.css inst/resources/
	Rscript -e "Rd2roxygen::rab('.', install=TRUE)"
	rm litedown_*.tar.gz
	cd tests && R CMD BATCH --no-save --no-restore --no-timing tests.R tests.Rout.save
