#' @title calculate mis
#' @description Internal function to calculate the mutual information between each known variable and latent variable(s).
#' @param data Data provided by user
#' @param theta List of estimated parameters
#' @param marginal_description Character string which determines the marginal distribution of the data. single marginal description applies to all variables in biocorex
#' @param p_y_given_x_3d A 3D array of numerics in range (0, 1), that represent the probability that each observed x variable belongs to n_hidden latent variables of dimension dim_hidden. p_y_given_x_3d has dimensions (n_hidden, n_samples, dim_hidden)
#' @param dim_visible The dimension of the data provided in data - i.e. the number of discrete levels that exist in the data. Must be positive integer.
#' @param log_p_y A 2D matrix representing the log of the marginal probability of the latent variables.
#' @return Returns an array of normalised mutual information with number of columns = n_visible and number of rows = n_hidden.
#'
#'@keywords internal
#'
calculate_mis <- function(data, theta, marginal_description, log_p_y, p_y_given_x_3d, dim_visible ){

    n_hidden <- dim(log_p_y)[1]
    dim_hidden <- dim(log_p_y)[2]
    n_samples <- dim(data)[1]
    n_visible <- dim(data)[2]

    mis <- array( rep(0, n_hidden * n_visible) , dim = c(n_hidden, n_visible))
    # randomly sample data
    samp <- sample( 1:nrow(data), nrow(data), replace = FALSE)
    n_observed <- colSums( !is.na( data[samp,] ) )

    # Calculate marginals
    log_marg_x_4d <- calculate_marginals_on_samples(data[samp,], theta, marginal_description,
                                                    log_p_y, dim_visible)

    #####
    # This section performs equivalent calculation to
    # numpy.einsum from Python with pattern 'ijl,ijkl->ik'

    # Random permute of p_y_given_x_3d
    samp_p_y_given_x_3d <- p_y_given_x_3d[, samp, , drop=FALSE]
    # Make emp ty array to hold result
    einsum_result <- array( rep(0, n_hidden * n_visible),
                            dim = c(n_hidden, n_visible))
    # Perform calculation
    for (i in 1: dim(log_marg_x_4d)[1]) {
                for( l in 1: dim(log_marg_x_4d)[4] ) {
                    einsum_result[i, ] <- einsum_result[i, ] +
                        samp_p_y_given_x_3d[i, , l] %*% log_marg_x_4d[i, , , l]
                }}
    mis <- einsum_result / n_observed

    return(mis)
}
