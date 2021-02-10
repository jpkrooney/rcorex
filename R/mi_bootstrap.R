#' @title mi_bootstrap
#' @description Internal function to calculate bias and sd of MIS via bootstrap
#' @param data Data provided by user
#' @param marginal_description Character string which determines the marginal distribution of the data.
#' @param theta List of estimated parameters
#' @param log_p_y A 2D matrix representing the log of the marginal probability of the latent variables
#' @param p_y_given_x_3d A 3D array of numerics in range (0, 1), that represent the probability that each observed x variable belongs to n_hidden latent variables of dimension dim_hidden. p_y_given_x_3d has dimensions (n_hidden, n_samples, dim_hidden).
#' @param dim_visible The dimension of the data provided in data - i.e. the number of discrete levels that exist in the data. Must be positive integer.
#' @param smooth_marginals Boolean (TRUE/FALSE) which indicates whether Bayesian smoothing of marginal estimates should be used.
#' @param n_permutes numeric to specify number of bootstrap estimates to calculate. Default = 20
#' @return Returns a list
#' @keywords internal
#'
mi_bootstrap <- function(data, marginal_description, minmarg, theta,
                         log_p_y, p_y_given_x_3d, dim_visible,
                         smooth_marginals, n_permutes=20){
    # Extract key data parameters
    n_hidden <- dim(p_y_given_x_3d)[1]
    n_samples <- dim(data)[1]
    n_visible <- dim(data)[2]
    mis <- array( rep(0, n_hidden*n_visible*n_permutes), dim = c(n_hidden, n_visible, n_permutes))

    # Re-calculate MIS using n_permutes re-samples of p_y_given_x_3d
    for( i in 1:n_permutes){
        samp <- sample(1:n_samples, size = n_samples, replace = FALSE)
        p_y_given_x_3d <- p_y_given_x_3d[, samp, , drop = FALSE]
        if( length(marginal_description) == 1 ){
            temp_theta <- calculate_theta(data, p_y_given_x_3d, marginal_description,
                                          smooth_marginals, dim_visible )
        } else {
            temp_theta <- calculate_theta_epi(data, p_y_given_x_3d, marginal_description,
                                              smooth_marginals)
        }
        mis[ , , i] <- calculate_mis(data, temp_theta, marginal_description, minmarg, log_p_y,
                                  p_y_given_x_3d, dim_visible )
    }

    bias <- apply(mis, c(1, 2), mean)

    # Calculate sig
    temp <- lapply( 1:n_visible, function(j) apply(mis[, j, , drop = FALSE], 1, sort))
    temp2 <- array( unlist(temp), dim = c(n_permutes, n_hidden, n_visible))
    sig <- temp2[19 , , ]

    out <- list(bias = bias, sig = sig)
    return(out)
}

