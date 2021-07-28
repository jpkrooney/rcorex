test_that("calculate_mis function calculates expected result", {

    # Run tests
    expect_equal(with_seed( calculate_mis(data = data, theta = theta_result_gaussian_no_smooth,
                                          marginal_description = "gaussian",
                                          log_p_y = log_p_y, p_y_given_x_3d = p_y_given_x_3d,
                                          dim_visible = NULL, logpx_method = "pycorex"), seed=555),
                 structure(c(-0.0941962762876255, 0.00954192205367097, -0.0941962762876255,
                             0.00954192205367097, -0.0941962762876255, 0.00954192205367097,
                             -0.0995261041131583, -0.000778754396565922, -0.0995261041131583,
                             -0.000778754396565922), .Dim = c(2L, 5L)))
})
