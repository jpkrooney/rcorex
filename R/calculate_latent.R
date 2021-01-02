#' @title calculate latent
#' @description Internal function to calculate the probability distribution for hidden factors for each sample
#' @details
#' Calculate the probability distribution for each latent y variable given the data, the marginals and the probability of each y. This calculation is represented by Equation 16 of \strong{Greg Ver Steeg and Aram Galstyan. Maximally Informative Hierarchical Representations of High-Dimensional Data. 2015.} \url{https://arxiv.org/abs/1410.7404 }
#' @param data Data provided by user
#' @param alpha 2D matrix. Numeric matrix of indicator variables that indicates adjacency of observed vs hidden variables
#' @param log_p_y A 2D matrix representing the log of the marginal probability of the latent variables
#' @param log_marg_x_4d A 4D array the contains the value of the marginal distribution for each variable, for each dimension of each hidden variable and for each data row.
#' @return 3D array of numerics in range (0, 1), that represent the probability for n_hidden latent y variables of dimension dim_hidden, for each observed x variable with dimensions (n_hidden, n_samples, dim_hidden)
#'
#'
calculate_latent <- function(data, alpha, log_p_y, log_marg_x_4d){
    # Get data and parameter dimensions
    n_hidden <- dim(log_p_y)[1]
    dim_hidden <- dim(log_p_y)[2]
    n_samples <- dim(data)[1]
    n_visible <- dim(data)[2]

    # Create 3D array to hold result
    log_p_y_given_x_unnorm <- array( dim =c(n_hidden, n_samples, dim_hidden ))

    #####
    # This section performs equivalent calculation to
    # numpy.einsum from Python with pattern 'ikl,ijkl->ijl'

    # to compute the einsum need to broadcast alpha along third dimension dim_hidden times
    alpha_3d <- array( dim = c(n_hidden, n_visible, dim_hidden))
    for(i in 1:dim_hidden){
        alpha_3d[, , i] <- alpha
    }
    # Create and array to hold result
    einsum_result <- array( rep(0, n_hidden * n_samples * dim_hidden),
                            dim = c(n_hidden, n_samples, dim_hidden))
    # Perform calculation
    for (i in 1: dim(log_marg_x_4d)[1]) {
            for( l in 1: dim(log_marg_x_4d)[4] ) {
                    einsum_result[i, , l] <- einsum_result[i, , l] +
                        log_marg_x_4d[i, , , l] %*% alpha_3d[i, , l]
                }}

    ##### Calculate log_p_y_given_x_unnorm
    log_p_y_given_x_unnorm_3d <- array( dim = c(n_hidden, n_samples, dim_hidden))
    for(i in 1: dim(einsum_result)[2] ){
        log_p_y_given_x_unnorm_3d[,i,] <- log_p_y + einsum_result[,i,]
    }

    # Re-normalise array
    out <- normalise_latent(log_p_y_given_x_unnorm_3d)
    return(out)
}
