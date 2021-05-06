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
    # check bernoulli algorithm on higher dimension data (6 by 6)
    expect_equal(estimate_parameters_bernoulli(x_i = databern6[, 1],
                                  p_y_given_x_3d = py_givenx_bern6x6,
                                  smooth_marginals = FALSE),
                 bern6result)



    # check bernoulli and discrete algorithm when n_hidden x dim_hidden is an odd number
    # for example 3x3 = 9, as certain potential speedups won't work in this case
    expect_equal(estimate_parameters_bernoulli(x_i = databern6[, 1],
                                    p_y_given_x_3d = array( c(py_givenx_bern6x6[ , , ]),
                                                            dim = c(3,6,3)),
                                    smooth_marginals = FALSE),
                 structure(c(-0.00222643435128376, -6.10846692943418, -0.694316546828901,
                             -0.691979180111471, -0.729418558613683, -0.658145496154548,
                             -6.9343730193069, -0.000974207775732681, -0.694680828435557,
                             -0.691615881158866, -0.642446382818512, -0.746556483689566,
                             -7.5888323059681, -0.000506199743397317, -0.693120211700754,
                             -0.693174150146476, -0.709390136288867, -0.677163847026783
                 ), .Dim = c(2L, 3L, 3L)) )

    expect_equal(estimate_parameters_discrete(x_i = databern6[, 1],
                                    p_y_given_x_3d = array( c(py_givenx_bern6x6[ , , ]),
                                                            dim = c(3,6,3)),
                                    smooth_marginals = FALSE, dim_visible = 2),
                 structure(c(-0.00222643435128376, -6.10846692943418, -0.694316546828901,
                             -0.691979180111471, -0.729418558613683, -0.658145496154548,
                             -6.9343730193069, -0.000974207775732681, -0.694680828435557,
                             -0.691615881158866, -0.642446382818512, -0.746556483689566,
                             -7.5888323059681, -0.000506199743397317, -0.693120211700754,
                             -0.693174150146476, -0.709390136288867, -0.677163847026783
                 ), .Dim = c(2L, 3L, 3L)) )




    # check discrete and bernoulli algorithm match on 6 by 6 data
    expect_equal(estimate_parameters_discrete(x_i = databern6[, 1],
                                    p_y_given_x_3d = py_givenx_bern6x6,
                                    smooth_marginals = FALSE, dim_visible = 2),
                 estimate_parameters_bernoulli(x_i = databern6[, 1],
                                               p_y_given_x_3d = py_givenx_bern6x6,
                                               smooth_marginals = FALSE))
    # check discrete algorithm on dim_vis =3, 7 x 7 data
    expect_equal(estimate_parameters_discrete(x_i = datadisc3_7 [, 1],
                                              p_y_given_x_3d = py_givenxdisc3_7,
                                              smooth_marginals = FALSE, dim_visible = 3),
                 disc3_7result)
})


