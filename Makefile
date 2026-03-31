all: check install

check:
	Rscript -e 'roxygen2::roxygenise()'
	R CMD build .
	R CMD check --no-manual *.tar.gz

install:
	R CMD INSTALL .

test:
	Rscript -e 'testthat::test_local()'

lint:
	Rscript -e 'l <- lintr::lint_package(); if (length(l) > 0L) { print(l); quit(status = 1) }'

coverage:
	Rscript -e 'cov <- covr::package_coverage(); print(cov); pct <- covr::percent_coverage(cov); if (pct < 85) stop(sprintf("Coverage %.1f%% is below 85%% threshold", pct))'

clean:
	rm -rf *.tar.gz *.Rcheck

.PHONY: all check install test lint coverage clean
