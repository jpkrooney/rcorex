#' @title calculate_p_y
#' @description Internal function to calculate the marginal probability of latent variables
#' @param p_y_given_x_3d A 3D array of numerics in range (0, 1), that represent the probability that each observed x variable belongs to n_hidden latent variables of dimension dim_hidden. p_y_given_x_3d has dimensions (n_hidden, n_samples, dim_hidden).
#' @return A 2D matrix representing the log of the marginal probability of the latent variables
#'
#'@keywords internal
#'
calculate_p_y <- function(p_y_given_x_3d){
    #Estimate log p(y_j) using a tiny bit of Laplace smoothing to avoid infinities.
    pseudo_counts_2d <- 0.001 + apply(p_y_given_x_3d, c(1, 3), sum)
    log_p_y <- log(pseudo_counts_2d) - log( rowSums( pseudo_counts_2d) )
    return(log_p_y)
}
