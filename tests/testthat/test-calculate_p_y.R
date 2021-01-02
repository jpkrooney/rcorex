context("calculate_p_y result check")
test_that("calculate_p_y returns expected result for debug data", {

    # Run tests
    expect_equal(calculate_p_y(p_y_given_x_3d), log_p_y)
})

