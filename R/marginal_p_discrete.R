#' @title Marginal Probabilities for discrete data
#' @description Internal function to estimate the marginals for discrete data.
#' @details
#' Estimate the marginals for a single column of data \code{x_i} for each hidden variable with dimension dim_hidden. For discrete data this can be calculated directly from the data.
#'
#' @param x_i A single variable/column of data
#' @param thetai Estimated parameters corresponding to the single variable/column of data provided to x_i
#' @param dim_visible The dimension of the variable provided in x_i - i.e. the number of discrete levels
#'
#' @return A three dimensional array of marginals with dimensions: \code{(n_hidden, dim_hidden, n_samples)} - i.e. marginals for each data point in x_i given current \emph{n_hidden x dim_hidden} parameter estimates
#' @keywords internal
#'
marginal_p_discrete <- function(x_i, thetai, dim_visible) {
    # Get parameter dimensions from parameters object
    n_hidden <- dim(thetai)[2]
    dim_hidden <- dim(thetai)[3]
    not_missing <- !is.na(x_i)

    # Extract estimates of 1:dim_visible parameters
    logp <- lapply(1: dim_visible, function(i) thetai[i, , ])

    # Make empty array to hold result
    z <- array( dim = c( n_hidden, dim_hidden, length(x_i)))
    # Calculate marginal directly
    for(i in 1:length( x_i[ not_missing ] ) ) {
        z[ , , i] <- logp[[ x_i[ not_missing ] [i] + 1] ]
    }
    z[ is.na(z) ] <- 0

    return(z)
}
