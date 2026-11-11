# Extracted from test-completion_model.R:41

# test -------------------------------------------------------------------------
skip_if_no_sequence()
p <- write_completion_parquet(completion_records())
con <- .disposition_lazy_con()
src <- .disposition_lazy_source(con, p, "test")
ctx <- .completion_context_tables(con, .completion_history_sql(con, src))
expect_setequal(unique(ctx$year), 2025:2027)
first <- ctx[ctx$context == "___" & is.na(ctx$survey_mode), ]
expect_equal(nrow(first), 3L)
expect_gt(first$n[first$year == 2027], first$n[first$year == 2025])
expect_true(all(ctx$p_complete >= 0 & ctx$p_complete <= 1))
