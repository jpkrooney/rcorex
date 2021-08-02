test_that("estimate_parameters_discrete result checks", {
    source(system.file("testdata", "testdata_estimate_parameters_discrete_plus_bernoulli.R",
                       package="rcorex", mustWork=TRUE))

    expect_equal(estimate_parameters_discrete(x_i = x_i, p_y_given_x_3d = p_y_given_x_3d,
                                              smooth_marginals = FALSE, dim_visible = 2),
                 result_discrete_no_smooth)

    expect_equal( estimate_parameters_discrete(x_i = data_est_tests[, 5],
                                               p_y_given_x_3d = p_y_given_x_3d_estimate_discrete,
                                               smooth_marginals = FALSE, dim_visible = 4),
                  est_pars_discrete_nosmooth_result)
    expect_equal( with_seed( 456, estimate_parameters_discrete(x_i = data_est_tests[, 5],
                                               p_y_given_x_3d = p_y_given_x_3d_estimate_discrete,
                                               smooth_marginals = TRUE, dim_visible = 4)),
                  est_pars_discrete_withsmooth_result)

    expect_equal(estimate_parameters_discrete(x_i = databern6[, 1],
                                    p_y_given_x_3d = array( c(py_givenx_bern6x6[ , , ]),
                                                            dim = c(3,6,3)),
                                    smooth_marginals = FALSE, dim_visible = 2),
                 est_pars_discrete_bern6_nosmooth_result )


    # check discrete algorithm on dim_vis =2, 6 x 6 data, nhid = dim_hid = 3
    expect_equal(estimate_parameters_discrete(x_i = databern6[, 1],
                                              p_y_given_x_3d = array( c(py_givenx_bern6x6[ , , ]),
                                                                      dim = c(3,6,3)),
                                              smooth_marginals = FALSE, dim_visible = 2),
                 bern6_nhid3dimhid3 )

    # check discrete algorithm on dim_vis =3, 7 x 7 data
    expect_equal(estimate_parameters_discrete(x_i = datadisc3_7 [, 1],
                                              p_y_given_x_3d = py_givenxdisc3_7,
                                              smooth_marginals = FALSE, dim_visible = 3),
                 disc3_7result)
})


