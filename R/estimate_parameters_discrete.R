#' @title estimate_parameters_discrete
#' @description Internal function to estimate parameters for data with discrete marginal description
#' @details Estimates the probability of each dim_visible occurring for each dim_hidden across each n_hidden.
#' @param x_i A single variable/column of data
#' @param p_y_given_x_3d A 3D array of numerics in range (0, 1), that represent the probability that each observed x variable belongs to n_hidden latent variables of dimension dim_hidden. p_y_given_x_3d has dimensions (n_hidden, n_samples, dim_hidden).
#' @param smooth_marginals Boolean (TRUE/FALSE) which indicates whether Bayesian smoothing of marginal estimates should be used as described in \strong{Pepke, S., Ver Steeg, G. Comprehensive discovery of subsample gene expression components by information explanation: therapeutic implications in cancer. BMC Med Genomics 10, 12 (2017).} \url{https://doi.org/10.1186/s12920-017-0245-6}
#' @param dim_visible The dimension of the data provided in data - i.e. the number of discrete levels that exist in the data. Must be positive integer.
#' @return Returns a 3D array of dimensions (dim_visible, n_hidden, dim_hidden) that represent the probability of each dim_visible occurring for each dim_hidden across each n_hidden.
#' @keywords internal
#'
estimate_parameters_discrete <- function(x_i, p_y_given_x_3d, smooth_marginals, dim_visible){

    # Get dimensions from p_y_given_x_3d -> in case of missing value may be different to expected
    n_hid <- dim(p_y_given_x_3d)[1]
    n_samp <- dim(p_y_given_x_3d)[2]
    n_dimhid <- dim(p_y_given_x_3d)[3]

    # Create boolean representation of data for each possible dim_visible value
    reference <- seq(0, dim_visible - 1)
    x_select <- matrix( rep(x_i, each=dim_visible) == reference, nrow = dim_visible)

    # matrix multiplication of x_select by p_y_given_x_3d
    #counts <- tensor(x_select, p_y_given_x_3d, alongA = 2, alongB=2) # dim_v, n_hid, dim_hid
    perm_p <- aperm(p_y_given_x_3d, c(2, 1, 3))
    dims_p <- dim(perm_p)
    dim(perm_p) <- c(dims_p[1], prod(dims_p[2:3]))
    counts <- x_select %*% perm_p
    dim(counts) <- c(dim_visible, n_hid, n_dimhid)

    # Add tiny smoothing to avoid numerical errors and make a copy
    #p <- counts + 0.001
    #denom <- apply(p, c(2,3), sum)
    p2 <- p <- counts + 0.001
    dim(p2) <- c(dim_visible, n_hid * n_dimhid)
    ## calculate denominator by getting colSums of p2
    denom <- matrix( colSums(p2), nrow=2)
    dim(denom) <- c(n_hid, n_dimhid)

    p <- p / rep(denom, each = dim_visible)

    if (smooth_marginals == TRUE){

        # Get prior as mean of data and count 'observations'
        prior <- rowMeans(x_select)
        #n_obs <- apply(p_y_given_x_3d, c(1 ,3 ), sum) # matrix 2D rows = n_hid, cols = n_dimhid
        temp <- aperm(p_y_given_x_3d, c(2 ,1, 3 ))
        dim(temp) <- c(dim(temp)[1], prod( dim(temp)[2:3]))
        n_obs <- matrix(colSums(temp), nrow = n_hid)

        # Shrinkage interpreted as hypothesis testing...
        term <- array( , dim = c(length(prior), dim(n_obs)))
        for(i in 1:length(prior)) {
            term[i, , ] <- log(n_obs * prior[i])
        }

        temp_term2 <- counts * (log(counts) - term)

        G_stat <- 2 * apply( ifelse( counts >0, temp_term2, 0 ) , c(2,3), sum)
        G0 <- estimate_sig(x_select, p_y_given_x_3d, term)
        z <- 1

        lam <- G_stat ^ z / (G_stat^z + G0^z)
        # Check for and replace any infinities
        lam[ is.infinite(lam) ] <- 0.5

        for (i in 1:dim_visible){
            p[i, , ] <- (1 - lam)*prior[i] +lam * p[i, , ]
        }
    }

    out <- log(p)
    return(out)
}
