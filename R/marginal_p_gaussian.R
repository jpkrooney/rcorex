#' @title Marginal Probabilities for Gaussian distributed data
#' @description Internal function to estimate the marginals for Gaussian distributed data.
#' @details
#' Estimate the marginals for a single column of data \code{x_i} for each hidden variable with dimension dim_hidden. For continuous data we estimate \eqn{p\left(y_{j} \mid x_{i}\right) / p\left(y_{j}\right)} indirectly by using Bayes rule to rewrite this as \eqn{p\left(x_{i} \mid y_{j}\right) / p\left(x_{i}\right)}, which can be estimated.
#'
#' @param x_i A single variable/column of data
#' @param thetai Estimated parameters corresponding to the single variable/column of data provided to x_i
#'
#' @return A three dimensional array of marginals with dimensions: \code{(n_hidden, dim_hidden, n_samples)} - i.e. marginals for each data point in x_i given current \emph{n_hidden x dim_hidden} parameter estimates
#' @keywords internal
#'
marginal_p_gaussian <- function(x_i, thetai) {
    # Get parameter dimensions from parameters object
    n_hidden <- dim(thetai[[1]])[1]
    dim_hidden <- dim(thetai[[1]])[2]
    n_samp <- length(x_i)

    # Extract Gaussian parameters to make code neater below
    mu <- thetai$mean_ml
    sig <- thetai$sig_ml

    # Pre-compute term for subtraction
    subtract_term <- 0.5 * log(2 * pi * sig)

    # Calculate marginals using parameters and formula for Gaussian distribution
    z <- -( rep(x_i, each = n_hidden*dim_hidden) - c((mu)) )^2
    z[is.na(z)] <- 0
    z <- (z / c(2*sig)) - c(subtract_term)
    # package into 3D array for return
    dim(z) <- c(n_hidden, dim_hidden, n_samp)
    #z <- aperm(z, c(1, 3, 2))

    return(z)
}
