all: check install

# Fast pre-commit gate: tests + lint + coverage in a single R session.
# Cuts ~10-15s of repeated cold startup vs running the three targets
# individually. Heavyweight gate is `make check` (R CMD check, ~30-60s).
verify:
	Rscript scripts/verify.R

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
	Rscript -e 'cov <- covr::package_coverage(); print(cov); pct <- covr::percent_coverage(cov); if (pct < 100) stop(sprintf("Coverage %.1f%% is below 100%% threshold", pct))'

e2e:
	Rscript e2e.R

# Hot-path perf harness (wall time + peak R heap per workload); appends to
# scripts/bench.jsonl. `make bench LABEL=after N=200000` to tag/scale a run.
LABEL ?= bench
N ?= 200000
bench:
	Rscript scripts/bench.R $(LABEL) $(N)

clean:
	rm -rf *.tar.gz *.Rcheck

.PHONY: all verify check install test lint coverage e2e bench clean
