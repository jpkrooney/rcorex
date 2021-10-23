#' @title estimate_se
#' @description Internal Function to calculate bootstrap estimate of mean and standard error of gaussian parameters
#' @param x_i A single variable/column of data
#' @param p_y_given_x_3d A 3D array of numerics in range (0, 1), that represent the probability that each observed x variable belongs to n_hidden latent variables of dimension dim_hidden. p_y_given_x_3d has dimensions (n_hidden, n_samples, dim_hidden).
#' @param num_obs A 2D array of dimensions (n_hidden, dim_hidden) representing expected counts of n_hidden x dim_hidden summed over data samples (rows) - can be non-integer
#' @param reps numeric to specify number of bootstrap estimates to calculate. Default = 20
#'
#' @return Returns a list of 4 numerics names m1, m2, se1, se2 which are used in smoothing calculations by \code{\link{estimate_parameters_gaussian}}
#' @keywords internal
#'
estimate_se <- function(x_i, p_y_given_x_3d, num_obs, reps = 20){
    # Get important data and parameter dimensions
    n_hid <- dim(p_y_given_x_3d)[1]
    n_samp <- dim(p_y_given_x_3d)[2]
    dim_hid <- dim(p_y_given_x_3d)[3]

    # Random reshuffle of data
    x_copy <- matrix(sample(x_i, reps *length(x_i), replace = TRUE),
                     nrow = length(x_i), ncol = reps )

    # Calculate ML estimate of means
    ml_means <- array(rep(0, n_hid * dim_hid * reps) , dim = c(n_hid, dim_hid, reps))
    for(j in 1: dim(p_y_given_x_3d)[1]) {
        for( k in 1: dim(p_y_given_x_3d)[3]) {
            ml_means[j, k, ] <- ml_means[j, k, ] +
                colSums(x_copy * p_y_given_x_3d[j, , k])
        }
    }
    ml_means <- ml_means / c(num_obs)
    m1 <- apply(ml_means, c(1, 2), mean)

    # Broadcast and reshape x_copy for 4D calculation
    x_copy4d <- replicate(n_hid *dim_hid, x_copy)
    dim(x_copy4d) <- c( dim(x_copy), n_hid, dim_hid)
    x_copy4d <- aperm(x_copy4d, c(3, 1, 4, 2))

    # Next make copy of ml_means for 4D calculation
    ml_means4d <- replicate(n_samp ,ml_means)
    ml_means4d <- aperm(ml_means4d, c(1, 4, 2, 3))

    # Now subtract ml_means4d from x_copy4d and square result
    term <- (x_copy4d - ml_means4d)^2

    # This section performs equivalent calculation to
    # numpy.einsum from Python with pattern 'jikl,jik->jkl'
    einsum_result <- array( rep(0, n_hid, dim_hid, reps), dim = c(n_hid, dim_hid, reps) )
    for(j in 1: dim(term)[1] ) {
            for(k in 1: dim(term)[3] ){
                    einsum_result[j, k, ] <- einsum_result[j, k, ] +
                        colSums(term[j, , k, ] * p_y_given_x_3d[j, , k])
            }
    }

    # Make denominator and set min value to 0.01 as per python code
    denominator <- num_obs - 1
    denominator[denominator < 0.01] <- 0.01

    ml_sigs <- einsum_result / c(denominator)
    # Calc m2 by averaging across reps
    m2 <- apply(ml_sigs, c(1, 2), mean)

    # Final step calculate standard errors for m1 and m2
    se1 <- sqrt(apply( (ml_means - c(m1)) ^2, c(1, 2), sum) / (0.95*reps) )
    se2 <- sqrt(apply( (ml_sigs - c(m2)) ^2, c(1, 2), sum) / (0.95*reps) )
    # possible bug in above - might need to be t(t(ml_means) - m1) etc

    # Return results
    out <- list(m1 = m1,
                m2 = m2,
                se1 = se1,
                se2 = se2)
    return(out)
}

