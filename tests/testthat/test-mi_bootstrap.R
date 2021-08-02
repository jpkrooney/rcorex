test_that("check mi_bootstrap works", {
    source(system.file("testdata", "testdata_mi_bootstrap.R",
                       package="rcorex", mustWork=TRUE))
    expect_equal( with_seed( mi_bootstrap(data = data_mi_bootstrap, marginal_description = "gaussian",
                              theta = theta_mi_bootstrap_gaussian,
                              log_p_y = log_p_y_mi_bootstrap,
                              p_y_given_x_3d = p_y_given_x_3d_mi_bootstrap,
                              dim_visible = NULL,
                              smooth_marginals = FALSE,
                              n_permutes = 20, logpx_method = "pycorex"),
                             seed = 934),
                  mi_bootstrap_gaussian_nosmooth_result)
})
