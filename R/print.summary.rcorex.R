#' @title Print a summary.rcorex object
#' @description Print method for a summary.rcorex object
#' @param x A object of class summary.rcorex
#' @param ... Not used
#' @export
#'
print.summary.rcorex <- function(x, ...) {
    # get parameters of latent and observed data

    cat("rcorex model call: \n")
    print(x$call)
    cat(paste0("Data dimensions: ", x$datadim[1], " samples (rows) by ", x$datadim[2], " variables (columns).\n"))
    cat(paste0("Latent variable parameters: rcorex searched for ", x$latentpars[1],
           " hidden variables with ", x$latentpars[2], " possible states.\n"))
    cat(paste0("Model outcome state: ", x$state, "\n"))
    cat(paste0("Numer of iterations performed: ", x$iters, "\n"))
    cat(paste0("Total TCS at final iteration: ", format(x$tcs, digits=5), "\n"))

}




