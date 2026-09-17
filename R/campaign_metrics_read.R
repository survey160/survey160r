# Campaign-metrics reader -- the R-only consumer of the consolidated
# per-campaign metrics projection.
#
# `campaign_all.parquet` is the fleet's per-campaign metrics + recipient-latency
# view: one consolidated Parquet the pipeline republishes on every producer run.
# It carries the funnel counts (n_sent / n_engaged / n_opted_in / n_completed),
# the recipient-latency distribution, and the `tracker_*` project dimensions
# (client, brand, registration_id, ...) per (campaign, date, hour). Unlike the
# disposition and opt-out projections it is NOT phone-keyed PII, so it lives in
# the analytics bucket and there is no screen helper -- callers read the pulled
# Parquet directly (arrow / nanoparquet) and aggregate.
#
# campaign_metrics_pull() stays bare and in the campaign_metrics family (not
# s160_-prefixed): it fetches a survey160r-derived artifact (the metrics
# projection the pipeline produces), not a raw Survey160 source -- the same rule
# that keeps disposition_pull / opt_out_pull bare.

#' Download the consolidated campaign-metrics projection from Cloud Storage
#'
#' Pulls \code{campaign_all.parquet} -- the fleet's consolidated per-campaign
#' metrics and recipient-latency view -- from the environment's analytics bucket
#' to a local file and returns the path, ready to read with your Parquet reader
#' of choice (e.g. \pkg{arrow} or \pkg{nanoparquet}). Downloaded once and reused
#' from the local cache on later calls (pass \code{refresh = TRUE} to force a
#' fresh download). Parallels \code{\link{disposition_pull}} and reaches GCS the
#' same way: authenticate first with \code{\link{s160_gcs_init}}() so the
#' session's GCS credentials are set. A download without an initialized session
#' errors with \dQuote{GCS not initialized. Run s160_gcs_init() first.} (a cache
#' hit is served without needing auth).
#'
#' The projection is one row per \code{(campaign_id, date, hour_local, segment,
#' threshold)} and carries the funnel counts, the recipient-latency
#' distribution, and the \code{tracker_*} project dimensions. It is aggregate
#' metrics, not phone-keyed PII, so it lives in the \code{s160_analytics_<env>}
#' bucket rather than the disposition bucket, and there is no screening helper --
#' read the pulled file and aggregate directly.
#'
#' @param env Environment: \code{"prod"} (default) or \code{"dev"}. There is no
#'   staging analytics tier; passing \code{env = "staging"} errors clearly.
#' @param dest Where to save. \code{NULL} (default) caches under
#'   \code{tools::R_user_dir("survey160r", "cache")}. A directory saves the
#'   default filename (\code{<bucket>.campaign_all.parquet}) inside it; any other
#'   single string is treated as the exact output path (its parent is created).
#' @param bucket \strong{Deprecated.} Select data with \code{env =} instead; a
#'   supplied bucket is honored with a warning for back-compat.
#' @param refresh When \code{FALSE} (default), reuse an existing local copy;
#'   \code{TRUE} always re-downloads (the projection is republished on every
#'   producer run, so refresh to pick up a newer one).
#' @param progress Show a download progress bar. Defaults to
#'   \code{interactive()}: a live bar in an interactive session, silent in batch
#'   or scheduled runs.
#' @return The local path to the downloaded Parquet (a single string).
#' @seealso \code{\link{disposition_pull}}, \code{\link{opt_out_pull}},
#'   \code{\link{s160_gcs_init}}
#' @examples
#' \dontrun{
#' s160_gcs_init()   # one-time browser OAuth
#' path <- campaign_metrics_pull()          # prod
#' metrics <- nanoparquet::read_parquet(path)
#' }
#' @export
campaign_metrics_pull <- function(env = .ENV_CHOICES, dest = NULL,
                                  bucket = NULL, refresh = FALSE,
                                  progress = interactive()) {
  env <- match.arg(env)
  loc <- .locate("campaign_metrics", env, bucket, "campaign_metrics_pull")
  .gcs_pull_cached(
    fn = "campaign_metrics_pull", dest = dest, bucket = loc$bucket,
    refresh = refresh, progress = progress,
    object_name = loc$object,
    cache_suffix = ".campaign_all.parquet", noun = "campaign metrics projection")
}
