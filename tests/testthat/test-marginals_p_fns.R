test_that("marginal_p functions produe correct results", {
    expect_equal( marginal_p_gaussian(x_i = x_i, thetai = theta_i_gaussian_ns),
                  marginal_p_gaussian_result )
    expect_equal( marginal_p_discrete(x_i = x_i, thetai = theta_i_discrete_ns, dim_visible = 2),
                  marginal_p_discrete_result )
    expect_equal( marginal_p_bernoulli(x_i = x_i, thetai = theta_i_bernoulli_ns),
                  marginal_p_bernoulli_result )
})
