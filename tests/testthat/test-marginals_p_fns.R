context("marginal_p functions check")
test_that("calculate_p_xi_given_y result checks", {
    expect_equal( marginal_p_gaussian(x_i = x_i, thetai = theta_i_gaussian_ns),
                  marginal_p_gaussian_result)
    expect_equal( marginal_p_discrete(x_i = x_i, thetai = theta_i_discrete_ns, dim_visible = 2),
                  marginal_p_discrete_result)

})
