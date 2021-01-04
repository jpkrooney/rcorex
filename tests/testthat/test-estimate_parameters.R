context("estimate_parameters check")
test_that("calculate_p_xi_given_y result checks", {
    expect_equal( estimate_parameters_discrete(x_i = data_est_tests[, 5],
                                               p_y_given_x_3d = p_y_given_x_3d_estimate_discrete,
                                               smooth_marginals = FALSE, dim_visible = 4),
                  est_pars_discrete_nosmooth_result)
    expect_equal( with_seed( 456, estimate_parameters_discrete(x_i = data_est_tests[, 5],
                                               p_y_given_x_3d = p_y_given_x_3d_estimate_discrete,
                                               smooth_marginals = TRUE, dim_visible = 4)),
                  est_pars_discrete_withsmooth_result)
    # Check if discrete and bernoulli produce the same answer for data with only 0 or 1
    expect_equal( estimate_parameters_discrete(x_i = data_est_tests[, 1],
                                               p_y_given_x_3d = p_y_given_x_3d_estimate_discrete,
                                               smooth_marginals = FALSE, dim_visible = 2),
                  estimate_parameters_bernoulli(x_i = data_est_tests[, 1],
                                                p_y_given_x_3d = p_y_given_x_3d_estimate_discrete,
                                                smooth_marginals = FALSE))
})
