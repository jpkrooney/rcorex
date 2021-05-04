context("update_alpha")
test_that("update_alpha calculation check", {
    expect_equal(with_seed( update_alpha(data = data, p_y_given_x_3d = p_y_given_x_3d, tcs = tcs,
                                                tc_min = tc_min, log_p_y = log_p_y,
                                                log_marg_x_4d_gaussian),
                                   seed = 5),
                 update_alpha_test_result_seed5)
})


