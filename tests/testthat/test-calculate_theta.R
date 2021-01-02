library(withr)

context("calculate_theta")
test_that("estimate_marginals functions calculate expected result", {

    # Run tests
    expect_equal(estimate_parameters_gaussian(x_i = x_i, p_y_given_x_3d = p_y_given_x_3d,
                                              smooth_marginals = FALSE), result_gaussian_no_smooth)
    expect_equal(with_seed( estimate_parameters_gaussian(x_i = x_i, p_y_given_x_3d = p_y_given_x_3d,
                                              smooth_marginals = TRUE), seed=5),
                 result_gaussian_smooth)
    expect_equal(estimate_parameters_discrete(x_i = x_i, p_y_given_x_3d = p_y_given_x_3d,
                                              smooth_marginals = FALSE, dim_visible = 2),
                 result_discrete_no_smooth)
})

test_that("calculate_theta produces expected result", {
    # Run tests
    expect_equal(calculate_theta(data = data, p_y_given_x_3d = p_y_given_x_3d,
                                 marginal_description = "gaussian", smooth_marginals = FALSE,
                                 dim_visible = NULL), theta_result_gaussian_no_smooth)
    expect_equal(calculate_theta(data = data, p_y_given_x_3d = p_y_given_x_3d,
                                 marginal_description = "discrete", smooth_marginals = FALSE,
                                 dim_visible = 2), theta_result_discrete_no_smooth)
})

