#' @title Summarise an rcorex object
#' @description Summary method for an rcorex object
#' @param object An object of class rcorex
#' @param ... Not used
#' @return Returns a list with the following model summary parameters:
#'\enumerate{
#' \item{call - the call used to run corex.}
#' \item{datadim - a vector enumerating the number of rows and columns in the data.}
#' \item{state - whether the model has converged or not.}
#' \item{iters - the number of iterations carried out by rcorex}
#' \item{tcs - a vector of TC for n_hidden variables.}
#' }
#' @export
#'
summary.rcorex <- function(object, ...) {
    # get parameters of latent and observed data
    n_hidden <- dim(object$p_y_given_x)[1]
    dim_hidden <- dim(object$p_y_given_x)[3]
    n_samples <- dim(object$p_y_given_x)[2]
    n_visible <- dim(object$alpha)[2]

    out <- list( call = object$call,
                 datadim = c(n_samples= n_samples, n_visible = n_visible),
                 latentpars = c(n_hidden = n_hidden, dim_hidden = dim_hidden),
                 state = object$state,
                 iters = object$iterations,
                 tcs = sum(object$tcs))
    class(out) <- "summary.rcorex"
    return(out)
}
