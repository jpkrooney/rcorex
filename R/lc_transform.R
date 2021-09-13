#'Transform an array of inputs, x, into an array of k latent factors, Y.
#'Optionally, you can get the remainder information and/or stop at a specified level
#

lc_transform <- function(data, details = FALSE, lincor_obj){

    ns = dim(data)[1]; nv = dim(data)[2]

    # Extract model components
    gaussianize <- lincor_obj$gaussianize
    weights <- lincor_obj$weights
    discourage_overlap <- lincor_obj$discourage_overlap
    eps <- lincor_obj$eps
    yscale <- lincor_obj$yscale
    theta <- lincor_obj$theta

    # Preprocess data
    processed = lc_preprocess(data, fit = FALSE, gaussianize = gaussianize, theta = theta)
    data <- processed$data
    theta <- processed$theta

    if( nv != ncol(weights)){
        stop("Incorrect number of variables in data compared to linearcorex fit.")
    }

    result <- data %*% t(weights)

    if(details == TRUE){
        moments <- lc_calculate_moments(data, weights, quick = FALSE,
                                        discourage_overlap, eps, yscale)
        return( list(labels = result,
                     moments = moments))
    } else {
        return( list(labels = result))
    }
}
