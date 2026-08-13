#' survey160r: R Client for Survey160 Data
#'
#' @description
#' Access Survey160 campaign data from R. The package has three layers: a raw
#' data-access layer that reaches Survey160's own systems (the \code{s160_*}
#' functions), and two higher-level surfaces built on top of it. \strong{Latency}
#' analysis is pure (in-memory data frame in, data frame out, no I/O).
#' \strong{Disposition} screening has a pure rollup core
#' (\code{\link{disposition_rollup}}), plus readers that access the Parquet
#' projection (\code{\link{disposition_summary}}, \code{\link{disposition_screen}},
#' \code{\link{disposition_records}}) or fetch it from GCS (\code{\link{disposition_pull}}).
#'
#' @section First-time setup:
#' Most functions that touch Survey160 data need a one-time sign-in:
#' \itemize{
#'   \item \code{\link{s160_gcs_init}} -- authenticate to Google Cloud Storage
#'     (a browser sign-in on first run; the token is cached). Needed to read
#'     campaign exports and to \code{\link{disposition_pull}} the disposition
#'     projection (when it must download; a cache hit needs no auth).
#'   \item \code{\link{s160_api_auth}} -- authenticate to the Survey160 REST API,
#'     only needed to trigger a \emph{fresh} export via
#'     \code{\link{s160_api_campaign_results}}.
#' }
#'
#' @section Disposition screening:
#' Answer "which of these phone numbers have we contacted / completed / refused
#' before?" and clean a sample list. Pull the shared disposition dataset once,
#' then screen a sample data frame in place:
#' \itemize{
#'   \item \code{\link{disposition_pull}} -- download the disposition projection
#'     from GCS (cached locally).
#'   \item \code{\link{disposition_screen}} -- append screening columns to your
#'     sample, 1:1 with its rows, preserving your original columns.
#'   \item \code{\link{disposition_summary}} / \code{\link{disposition_records}}
#'     -- ad-hoc query surfaces over the same dataset (one row per phone / one
#'     row per \code{(phone, campaign_id)}); \code{\link{disposition_rollup}}
#'     rolls an already-read frame up in memory.
#' }
#'
#' @section Latency analysis:
#' Compute a per-campaign recipient-latency report from a raw campaign CSV:
#' \itemize{
#'   \item \code{\link{latency_run}} -- the one-campaign runner (builds the
#'     config from the CSV header, then runs the algorithm).
#'   \item \code{\link{latency_report}} -- the pure algorithm, for tests and
#'     ad-hoc analysis.
#'   \item \code{\link{s160_gcs_pull_csv}} / \code{\link{s160_read_csv}} -- read a
#'     campaign export from GCS or from a local file, respectively.
#' }
#'
#' Persisting outputs as Parquet, walking the whole campaign fleet, and
#' scheduling all live in a downstream consumer project -- this package is
#' algorithm-only and source-agnostic.
#'
#' @keywords internal
"_PACKAGE"
