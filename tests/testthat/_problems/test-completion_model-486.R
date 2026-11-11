# Extracted from test-completion_model.R:486

# prequel ----------------------------------------------------------------------
mixed_size_records <- function() {
  # odd campaigns keep 60 phones, even ones 40: two size bands around a break of 50
  r <- completion_records(n_per_campaign = 60L)
  keep <- unlist(lapply(split(seq_len(nrow(r)), r$campaign_id), function(i) {
    if (r$campaign_id[i[1L]] %% 2L == 0L) i[1:40] else i
  }))
  r[sort(keep), ]
}

# test -------------------------------------------------------------------------
skip_if_no_sequence()
p <- write_completion_parquet(completion_records())
con <- .disposition_lazy_con()
src <- .disposition_lazy_source(con, p, "test")
history <- .completion_history_sql(con, src)
ctx <- .completion_context_tables(con, history)
expect_true(all(c("p_complete", "p_reply") %in% names(ctx)))
expect_true(all(ctx$p_reply >= 0 & ctx$p_reply <= 1))
known <- !is.na(ctx$p_complete)
expect_true(all(ctx$p_reply[known] >= ctx$p_complete[known]))
