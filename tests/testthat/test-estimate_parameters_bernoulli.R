test_that("estimate_parameters_bernoulli result checks", {
    source(system.file("testdata", "testdata_estimate_parameters_discrete_plus_bernoulli.R",
                       package="rcorex", mustWork=TRUE))

    # check bernoulli algorithm on higher dimension data (6 by 6)
    expect_equal(estimate_parameters_bernoulli(x_i = databern6[, 1],
                                               p_y_given_x_3d = py_givenx_bern6x6,
                                               smooth_marginals = FALSE),
                 bern6resultnosmooth)
    expect_equal( with_seed( estimate_parameters_bernoulli(x_i = databern6[, 1],
                                               p_y_given_x_3d = py_givenx_bern6x6,
                                               smooth_marginals = TRUE),
                           seed=987),
                 bern6resultwithsmooth)

    # Check if discrete and bernoulli produce the same answer for data with only 0 or 1
    expect_equal( estimate_parameters_discrete(x_i = data_est_tests[, 1],
                                               p_y_given_x_3d = p_y_given_x_3d_estimate_discrete,
                                               smooth_marginals = FALSE, dim_visible = 2),
                  estimate_parameters_bernoulli(x_i = data_est_tests[, 1],
                                                p_y_given_x_3d = p_y_given_x_3d_estimate_discrete,
                                                smooth_marginals = FALSE))

    expect_equal(  with_seed( estimate_parameters_discrete(x_i = data_est_tests[, 1],
                                               p_y_given_x_3d = p_y_given_x_3d_estimate_discrete,
                                               smooth_marginals = TRUE, dim_visible = 2),
                              seed = 674),
                   with_seed( estimate_parameters_bernoulli(x_i = data_est_tests[, 1],
                                                p_y_given_x_3d = p_y_given_x_3d_estimate_discrete,
                                                smooth_marginals = TRUE),
                              seed = 674))

    # check bernoulli and discrete algorithm when n_hidden x dim_hidden is an odd number
    # for example 3x3 = 9, as certain potential speedups won't work in this case
    expect_equal(estimate_parameters_bernoulli(x_i = databern6[, 1],
                                               p_y_given_x_3d = array( c(py_givenx_bern6x6[ , , ]),
                                                                       dim = c(3,6,3)),
                                               smooth_marginals = FALSE),
                 bern6_nhid3dimhid3)
    expect_equal( with_seed( estimate_parameters_bernoulli(x_i = databern6[, 1],
                                               p_y_given_x_3d = array( c(py_givenx_bern6x6[ , , ]),
                                                                       dim = c(3,6,3)),
                                               smooth_marginals = FALSE),
                             seed = 290),
                  bern6_nhid3dimhid3_withsmooth)

    # check discrete and bernoulli algorithm match on 6 by 6 data
    expect_equal(estimate_parameters_discrete(x_i = databern6[, 1],
                                              p_y_given_x_3d = py_givenx_bern6x6,
                                              smooth_marginals = FALSE, dim_visible = 2),
                 estimate_parameters_bernoulli(x_i = databern6[, 1],
                                               p_y_given_x_3d = py_givenx_bern6x6,
                                               smooth_marginals = FALSE))
})


