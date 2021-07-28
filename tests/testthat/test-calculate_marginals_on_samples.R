test_that("calculate_marginals_on_samples works correctly", {
    expect_equal(calculate_marginals_on_samples(data = data,
                                                theta = theta_result_gaussian_no_smooth,
                                                marginal_description = "gaussian",
                                                log_p_y = log_p_y,
                                                dim_visible = dim_visible,
                                                logpx_method = "pycorex"),
                 log_marg_x_4d_gaussian)

    expect_equal(calculate_marginals_on_samples(data = data,
                                                theta = theta_result_discrete_no_smooth,
                                                marginal_description = "discrete",
                                                log_p_y = log_p_y,
                                                dim_visible = dim_visible,
                                                logpx_method = "pycorex"),
                 log_marg_x_4d_discrete)
})
