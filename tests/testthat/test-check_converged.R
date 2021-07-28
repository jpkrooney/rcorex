test_that("check_converged works", {
    expect_equal(check_converged(tc_hist_converged, eps=1e-5), TRUE)
    expect_equal(check_converged(tc_hist_converged, eps=1e-7), FALSE)
    expect_equal(check_converged(tc_hist_converged[1:5], eps=1e-5), FALSE)
})
