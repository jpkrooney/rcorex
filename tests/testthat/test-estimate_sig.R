context("estimate_sig")
test_that("estimate_sig function calculates expected result", {

    # Run tests
    expect_equal(with_seed( estimate_sig(x_select = sig_x_select, p_y_given_x_3d = py_givenxdisc3_7,
                                         term = sig_term), seed=555),
                 estsig_result)
})
