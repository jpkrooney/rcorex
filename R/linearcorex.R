#' @title Fit linearcorex to a dataset
#' @description EXPERIMENTAL implmentation of linearcorex algorithm
#' @details
#' Experimental implementation of  \url{https://github.com/gregversteeg/LinearCorex}. Currently not fully tested.
#' @param data Data provided by user.
#' @param n_hidden An integer number of hidden variables to search for. Default = 10.
#' @param max_iter numeric. Maximum number of iterations. Default = 10000
#' @param tol Tolerance
#' @param anneal Default = TRUE
#' @param discourage_overlap Default = TRUE
#' @param gaussianize Default = standard
#' @param verbose Default = FALSE
#' @return Returns linearcorex fit objects as a list
#' @export
#'

linearcorex <- function(data, n_hidden = 10, max_iter = 10000, tol = 1e-5,
                        anneal = TRUE,  # missing_values=None,
                        discourage_overlap = TRUE, gaussianize = 'standard', # gpu=False,
                        verbose = FALSE){

    if (verbose == TRUE){
        message("Linear CorEx with ", n_hidden, " latent factors")
    }

    m <- n_hidden  # Number of latent factors to learn
    n_samples <- nrow(data) ; nv <- ncol(data)
    res <- lc_preprocess(data, TRUE, gaussianize)
    data <- res$data
    theta <- res$theta

    # Initialize parameters
    TC <- 0
    histlc <- list(TC = list(),
                   TCs = list(),
                   additivity = list()) # For recording parameters each step

    eps <- 0  # If anneal is True, it's adjusted during optimization to avoid local minima
    yscale <- 1  # Can be arbitrary, but sets the scale of Y
    anneal_schedule <- 0

    #if (is.null(m)){ # bookmarked for later - may not work in R context
    #    m <- pick_n_hidden()
    #}

    # Initialize weights
    if(discourage_overlap == TRUE){
        weights <- matrix( rnorm(m*nv), nrow = m)
        weights <- weights / (10 * lc_norm(data, weights, eps))
        if(anneal == TRUE){
            anneal_schedule <- 0.6^(1:7)
        }
    } else {
        weights <- matrix( rnorm(m*nv), nrow = m) * yscale^2 / sqrt(nv)
    }

    # Calculate initial moments
    moments <- lc_calculate_moments(data, weights, quick = FALSE,
                         discourage_overlap, eps, yscale)

    for(i in 1: length(anneal_schedule) ){
        # We change epsilon, the annealing parameter, but we have to scale w to ensure that
        # it is still a valid solution (m['uj'] < 1), even if there are numerical rounding issues.
        eps0 <- eps
        eps <- anneal_schedule[i]

        if(i > 1){  # Rescale the solution when we change the annealing parameter
            wmag <- rowSums( weights^2 )
            delta <- (eps^2 - eps0^2) / (1 - eps^2) *wmag / moments$uj
            a <-sqrt((1 - eps0^2) / ( (1 - eps^2)*(1 + delta) ))
            weights <- weights* 0.001 * floor(1000 * a)
        }
        moments = lc_calculate_moments(data, weights, quick = FALSE, discourage_overlap, eps, yscale)

        for(i_loop in 1: max_iter){
            last_TC = TC
            if(discourage_overlap == TRUE){
                updated <- lc_update_ns(data, moments, weights, eps, tol, yscale, verbose)
                weights <- updated$w_update
                moments <- updated$m_update
                TC <- moments$TC
            } else {
                # Older method that allows synergies
                updated <- lc_update_syn(data, moments, weights, eps, tol, yscale, verbose, eta = 0.1)
                weights <- updated$w_update
                moments <- updated$m_update
                TC <- moments$TC
            }
            if(is.logical(moments) | !is.finite(TC)){
                stop("Error: moments are invalid as TC is infinite")
            }
            delta <- abs(TC - last_TC)
            histlc <- lc_update_records(histlc, moments, delta, verbose)

            # Check for convergence
            if(delta < tol){
                if(verbose == TRUE){
                    print(paste0(i_loop, " iterations to tol: ", tol, ", TC = ", TC))
                }
                break
            }
            #else:
            #    if self.verbose:
            #    print(("Warning: Convergence not achieved in {:d} iterations. "
            #           "Final delta: {:f}".format(self.max_iter, delta.sum())))
            #
       }
    }

    # Update moments with details
    moments = lc_calculate_moments(data, weights, quick = FALSE,
                                   discourage_overlap, eps, yscale)
    ord <- order(moments$TCs, decreasing = TRUE)
    weights <- weights[ord,]
    # Update moments based on sorted weights
    moments <- lc_calculate_moments(data, weights, quick = FALSE,
                                   discourage_overlap, eps, yscale)

    # Assign clusters
    clusters <- max.col(t(abs(weights)))

    # Package results for return to user
    out <- list(
        iters = i_loop,
        gaussianize = gaussianize,
        theta = theta,
        weights = weights,
        moments = moments,
        clusters = clusters,
        histlc = histlc,
        discourage_overlap = discourage_overlap,
        eps = eps,
        yscale = yscale
    )
    return(out)

}



# Helper function
# called _norm in python version
lc_norm <- function(data, weights, eps){
    #Calculate uj so that we can normalize it.
    #y = x.dot(ws.T)  # + noise / std Y_j^2, but it is included analytically
    #tmp_sum = np.einsum('lj,lj->j', y, y)
    #return np.sqrt((1 - self.eps**2) * tmp_sum / self.n_samples + self.eps**2 * np.sum(ws**2, axis=1))
    y <- data %*% t(weights)
    tmp_sum <- colSums(y^2)
    return( sqrt( (1 - eps^2 ) * tmp_sum / nrow(data) + eps^2 * rowSums(weights^2) ) )
}



