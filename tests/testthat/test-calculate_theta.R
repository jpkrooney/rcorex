context("calculate_theta")
test_that("calculate_theta produces expected result", {
    # Run tests
    expect_equal(calculate_theta(data = data, p_y_given_x_3d = p_y_given_x_3d,
                                 marginal_description = "gaussian", smooth_marginals = FALSE,
                                 dim_visible = NULL), theta_result_gaussian_no_smooth)
    expect_equal(calculate_theta(data = data, p_y_given_x_3d = p_y_given_x_3d,
                                 marginal_description = "discrete", smooth_marginals = FALSE,
                                 dim_visible = 2), theta_result_discrete_no_smooth)
})

