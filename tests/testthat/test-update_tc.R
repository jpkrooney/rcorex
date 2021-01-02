context("update_tc")
test_that("update_tc calculation check", {
    expect_equal(update_tc(log_z = updated_log_z, tcs = tcs, tc_history = tc_history),
                 updated_tc)
})
