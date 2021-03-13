context("check_converged")
test_that("check check_converged works", {
    expect_equal(check_converged(tc_hist_converged, eps=1e-5), TRUE)
    expect_equal(check_converged(tc_hist_converged, eps=1e-7), FALSE)
})
