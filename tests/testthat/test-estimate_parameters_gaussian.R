context("estimate_parameters_gaussian")
test_that("estimate_parameters_gaussian functions calculate expected result", {

    # Run tests
    expect_equal(estimate_parameters_gaussian(x_i = x_i, p_y_given_x_3d = p_y_given_x_3d,
                                              smooth_marginals = FALSE), result_gaussian_no_smooth)
    expect_equal(with_seed( estimate_parameters_gaussian(x_i = x_i, p_y_given_x_3d = p_y_given_x_3d,
                                                         smooth_marginals = TRUE), seed=5),
                 result_gaussian_smooth)
})
