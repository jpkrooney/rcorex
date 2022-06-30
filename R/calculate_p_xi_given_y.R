#' @title calculate_p_xi_given_y
#' @description Internal function to calculate marginals for a single column of data given parameter estimates
#' @details
#' This function is an intermediate function with parent \code{\link{calculate_marginals_on_samples}} and child functions \code{\link{marginal_p_discrete}} and \code{\link{marginal_p_gaussian}}. It's main function is to call the appropriate child function depending on the value of the \code{marginal_description} variable.
#' @param x_i A single variable/column of data
#' @param thetai Estimated parameters corresponding to the single variable/column of data provided to x_i
#' @param marginal_description Character string which determines the marginal distribution of the data. For biocorex only 'gaussian' and 'discrete' are allowed.
#' @param dim_visible Integer. The dimension of the data - i.e. the number of discrete levels that exist in the data.
#' @return A 3D array of marginals with dimensions: \code{(n_hidden, n_samples, dim_hidden)} - i.e. marginals for each data point in x_i given current \emph{n_hidden x dim_hidden} parameter estimates
#'
#'@keywords internal
#'
calculate_p_xi_given_y <- function(x_i, thetai, marginal_description, dim_visible = NULL){

    if(marginal_description == "gaussian"){
        z <- marginal_p_gaussian(x_i, thetai)

    } else if(marginal_description == "discrete"){
        z <- marginal_p_discrete(x_i, thetai, dim_visible)

    } else if(marginal_description == "discrete_byvar"){
        z <- marginal_p_discrete(x_i, thetai)

    } else if(marginal_description == "bernoulli"){
        z <- marginal_p_bernoulli(x_i, thetai)

    } else {
        stop("That marginal description is not impelemented")
    }

    return(z)
}

