tests: tests/tests.Rout.save

tests/tests.Rout.save: tests/tests.R inst/examples/render-options.R
	ln -f ../misc.js/js/snap.js ../misc.js/css/snap.css inst/resources/
	Rscript -e "Rd2roxygen::rab('.', install=TRUE)"
	rm litedown_*.tar.gz
	cd tests && R CMD BATCH --no-save --no-restore --no-timing tests.R tests.Rout.save
