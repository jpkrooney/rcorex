test_that("check calculate_latent works", {
    source(system.file("testdata", "testdata_calculate_marginals_on_samples.R",
                       package="rcorex", mustWork=TRUE))
    expect_equal(calculate_latent(data = data, alpha = alpha_4latent_test, log_p_y = log_p_y,
                                  log_marg_x_4d = log_marg_x_4d_gaussian)$p_y_given_x_3d,
                 updated_p_y_given_x_3d)
    expect_equal(calculate_latent(data = data, alpha = alpha_4latent_test, log_p_y = log_p_y,
                                  log_marg_x_4d = log_marg_x_4d_gaussian)$log_z,
                 updated_log_z)
})
