context("normalise_latent check")
test_that("normalise_latent produces expected values for debug data", {
    expect_equal(normalise_latent(log_p_y_given_x_unnorm_3d = p_rand)$p_y_given_x_3d, p_y_given_x_3d)
    expect_equal(normalise_latent(log_p_y_given_x_unnorm_3d = p_rand)$log_z, log_z)
})
