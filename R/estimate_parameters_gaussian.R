#' @title estimate_parameters_gaussian
#' @description Internal function to estimate parameters for data with gaussian marginal description
#' @param x_i A single variable/column of data
#' @param p_y_given_x_3d A 3D array of numerics in range (0, 1), that represent the probability that each observed x variable belongs to n_hidden latent variables of dimension dim_hidden. p_y_given_x_3d has dimensions (n_hidden, n_samples, dim_hidden).
#' @param smooth_marginals Boolean (TRUE/FALSE) which indicates whether Bayesian smoothing of marginal estimates should be used as described in \strong{Pepke, S., Ver Steeg, G. Comprehensive discovery of subsample gene expression components by information explanation: therapeutic implications in cancer. BMC Med Genomics 10, 12 (2017).} \url{https://doi.org/10.1186/s12920-017-0245-6}
#' @importFrom stats sd
#' @return Returns a list of 2 matrices each of dimension (n_hidden, dim_hidden). The first matrix represents estimated means for n_hidden x dim_hidden parameters, while the second matrix represents corresponding standard deviations.
#' @keywords internal
#'
estimate_parameters_gaussian <- function(x_i, p_y_given_x_3d, smooth_marginals){

    # Get dimensions from p_y_given_x_3d -> in case of missing value may be different to expected
    n_hid <- dim(p_y_given_x_3d)[1]
    n_samp <- dim(p_y_given_x_3d)[2]
    n_dimhid <- dim(p_y_given_x_3d)[3]

    # Calculate num_obs and set minimum value
    num_obs <- colSums(aperm(p_y_given_x_3d, c(2, 1, 3)))
    num_obs[num_obs < 0.1] <- 0.1   # min value num_obs is 0.1 to avoid divide by 0

    # calculate mean_ml - matrix 2D rows = n_hid, cols = n_dimhid
    tmp <- aperm(p_y_given_x_3d, c(2, 1, 3))
    dim(tmp) <- c(n_samp, n_hid*n_dimhid)

    mean_ml <- x_i %*% tmp
    dim(mean_ml) <- c(n_hid, n_dimhid)
    mean_ml <- mean_ml / num_obs

    # calculate sig_ml by steps
    term <- (x_i - rep(mean_ml, each=n_samp))
    dim(term) <- c(n_samp, n_hid, n_dimhid)
    term <- aperm(term, c(2, 1, 3))

    term <- term^2 # Element wise squaring of term
    term2 <- term * p_y_given_x_3d  # Element wise multiplication
    denominator <- num_obs - 1
    denominator[denominator < 0.01] <- 0.01     # min value 0.01 to avoid divide by zero

    sig_ml <- colSums(aperm(term2, c(2, 1, 3))) / denominator
    sig_ml[sig_ml < .Machine$double.xmin] <- .Machine$double.xmin # min value to avoid divide by zero

    ##### add option for smooth marginals later
    if (smooth_marginals == TRUE){
        mean0 <- mean(x_i)
        sig0 <- sd(x_i)^2

        params <- estimate_se(x_i, p_y_given_x_3d, num_obs)
        m1 <- params$m1
        m2 <- params$m2
        se1 <- params$se1
        se2 <- params$se2
        d1 <- mean_ml - m1
        d2 <- sig_ml - m2
        lam <- d1^2 / (d1^2 + se1^2)
        gam <- d2^2 / (d2^2 + se2^2)

        # Check for and replace any infinities
        lam[ is.infinite(lam) ] <- 0.5
        gam[ is.infinite(gam) ] <- 0.5

        mean_prime <- lam * mean_ml + (1 - lam) * mean0
        sig_prime <- gam * sig_ml + (1 - gam) * sig0

        out <- list(mean_ml = mean_prime, sig_ml = sig_prime)
    } else {
        out <- list(mean_ml = mean_ml, sig_ml = sig_ml)
    }

    return(out)
}
