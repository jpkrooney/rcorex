#' @title normalise_latent
#' @description Internal function to normalise the latent variable distribution
#' @param log_p_y_given_x_unnorm_3d A 3D array with dimensions (n_hidden, n_samples, dim_hidden) containing the un-normalised latent variable distribution.
#' @return Returns a list called out which contains two elements:
#' \enumerate{
#' \item {A 3D array of numerics in range (0, 1), that represent the probability that each observed x variable belongs to n_hidden latent variables of dimension dim_hidden. p_y_given_x_3d has dimensions (n_hidden, n_samples, dim_hidden).}
#' \item{A 2D matrix containing the pointwise estimate of total correlation explained by each latent variable for each sample.}
#' }
#'
normalise_latent <- function(log_p_y_given_x_unnorm_3d){

    # Calculate log_z via logSumExp collapsing dim[3]
    temp <- aperm(log_p_y_given_x_unnorm_3d, c(3, 1, 2))
    dim(temp) <- c(dim(temp)[1], prod(dim(temp)[-1] ))
    cmaxs <- colMaxs(temp)
    log_z <- cmaxs + log(rowSums(exp( t(temp) - cmaxs ) ))
    log_z <- matrix( log_z, dim(log_p_y_given_x_unnorm_3d)[1:2])

    # Normalise latent
    p_y_given_x_3d <- exp( log_p_y_given_x_unnorm_3d - c(log_z) )

    out <- list(p_y_given_x_3d = p_y_given_x_3d, log_z = log_z)
    return(out)
}
