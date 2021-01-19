#' @title update_alpha
#' @description Internal function to update alpha - the adjacency matrix that represents connections between input variables and hidden variables.
#' @param data Data provided by user.
#' @param p_y_given_x_3d A 3D array of numerics in range (0, 1), that represent the probability of n_hidden latent variables of dimension dim_hidden, for each observed x variable with dimensions (n_hidden, n_samples, dim_hidden).
#' @param tcs Vector of length n_hidden - contains the TC for each hidden factor. This is used to decided the sort order for all the other parameters such that hidden factors are returned to used in order of largest TC to smallest TC.
#' @param tc_min Minimum value which is allowed for TC for any given hidden variable.
#' @param log_p_y A 2D matrix representing the log of the marginal probability of the latent variables.
#' @param log_marg_x_4d A 4D array that contains the value of the marginal distribution for each variable, for each dimension of each hidden variable and for each data row.
#' @return Returns an updated alpha matrix - an adjacency matrix between input variables and hidden variables.
#'
update_alpha <- function(data, p_y_given_x_3d, tcs, tc_min, log_p_y, log_marg_x_4d){

    # Extract important dimensions regarding data and hidden factors
    n_hidden = dim(p_y_given_x_3d)[1]
    n_visible = dim(data)[2]
    dim_hidden = dim(p_y_given_x_3d)[3]

    # Random sample row order of data
    samp <- sample( 1:nrow(data), nrow(data), replace = FALSE)

    # Subset p_y_given_x_3d as per random sample
    p_y_given_x <- p_y_given_x_3d[, samp, ]

    # make empty alpha of n_hidden by n_visible
    alpha = matrix(nrow = n_hidden, ncol = n_visible)

    # get indexes of maximal values of log_marg_x_4d with respect to 4th dimesion
    max_marg <- which_max_4D( log_marg_x_4d )

    # get indexes of maximal values of p_y_given_x_3d with respect to 3rd dimension
    max_idx2 <- which_max_3D( p_y_given_x_3d )

    # Cross-reference max_indexes of p_y_given_x_3d with log_marg_x_4d for each dim_hidden
    correct_predictions <- lapply(1:dim(log_marg_x_4d)[3], function(i) max_idx2 == max_marg[ , , i])

    # Calculate alpha using unique_info function looped over n_visible
    alpha <- do.call( "cbind", lapply(1:n_visible, function(i) {
        not_missing <- !is.na(data[, i])
        matrix( unique_info(correct = correct_predictions[[i]] [, not_missing] ),
                ncol= 1, nrow = n_hidden ) }))

    # adjust alpha where tcs < tc_min to boost unused observed vs hidden variable adjacency
    condition <- which( abs(tcs) < tc_min )
    for (i in condition){
        amax <- pmin( pmax( max(alpha[i, ]) , 0.01), 0.99)
        alpha[i, ] <- alpha[i, ]^(log(0.99)/log(amax) ) + 0.001 * runif(n_visible, 0, 1)
    }

    return(alpha)
}

# Support functions
which_max_4D <- function(x) {
    slice <- dim(x)[1:3]
    res1 <- array(integer(1), dim = slice)
    num <- prod(slice)
    span <- seq_len( dim(x)[4] ) * num - num
    for(i in seq_len(num))
        res1[i] <- which.max(x[i + span])
    return(res1)
}

which_max_3D <- function(x) {
    slice <- dim(x)[1:2]
    res1 <- array(integer(1), dim = slice)
    num <- prod(slice)
    span <- seq_len( dim(x)[3] ) * num - num
    for(i in seq_len(num))
        res1[i] <- which.max(x[i + span])
    return(res1)
}








