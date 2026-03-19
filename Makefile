all: check install

check:
	Rscript -e 'roxygen2::roxygenise()'
	R CMD build .
	R CMD check --no-manual *.tar.gz

install:
	R CMD INSTALL .

test:
	Rscript -e 'testthat::test_local()'

clean:
	rm -rf *.tar.gz *.Rcheck

.PHONY: all check install test clean
