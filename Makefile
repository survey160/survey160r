all: check install

# Test files run in parallel workers (Config/testthat/parallel in DESCRIPTION).
# testthat starts 2 unless told otherwise; `make test` and `make coverage` use
# the cores, up to 8 (past that the suite stops getting faster).
# `make test TESTTHAT_CPUS=1` runs the files one after another.
TESTTHAT_CPUS ?= $(shell n=$$(nproc 2>/dev/null || echo 2); if [ "$$n" -gt 8 ]; then n=8; fi; echo $$n)

# Fast pre-commit gate: tests + lint + coverage, the three at the same time
# (scripts/verify.R), so it takes as long as its slowest phase. It picks its
# own worker count (tests and coverage share the cores) unless TESTTHAT_CPUS
# is set in the environment. Heavyweight gate is `make check` (R CMD check).
verify:
	Rscript scripts/verify.R

check:
	Rscript -e 'roxygen2::roxygenise()'
	R CMD build .
	R CMD check --no-manual *.tar.gz

install:
	R CMD INSTALL .

test:
	TESTTHAT_CPUS=$(TESTTHAT_CPUS) Rscript -e 'testthat::test_local()'

# One test file, package loaded once: `make test-file FILE=tests/testthat/test-latency_run.R`
test-file:
	Rscript -e 'pkgload::load_all(".", quiet = TRUE); testthat::test_file("$(FILE)")'

# lintr's cache keeps the lints of unchanged expressions (every configured
# linter works one expression or file at a time, so a cached result cannot go
# stale). `make lint LINT_CACHE=FALSE` lints from scratch, as CI does.
LINT_CACHE ?= TRUE
lint:
	Rscript -e 'l <- lintr::lint_package(cache = $(LINT_CACHE)); if (length(l) > 0L) { print(l); quit(status = 1) }'

coverage:
	TESTTHAT_CPUS=$(TESTTHAT_CPUS) Rscript -e 'cov <- covr::package_coverage(); print(cov); pct <- covr::percent_coverage(cov); if (pct < 100) stop(sprintf("Coverage %.1f%% is below 100%% threshold", pct))'

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

.PHONY: all verify check install test test-file lint coverage e2e bench clean
