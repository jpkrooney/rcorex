#' @title initialise_parameters
#' @description Internal function to initialise corex parameters
#' @details Given the main corex arguments (the data, n_hidden and dim_hidden) this function generates objects with the correct dimensions to initialise the corex algorithm. Random values for alpha are chosen from a random uniform bound between 0 and 1, while for the unnormalised p_y_given_x_3d random values are chosen from a dirichlet distribution before p_y_given_x_3d is then normalised - during this step initial values for log_z are also calculated.
#' @param data Data provided by user
#' @param n_hidden integer. Integer number of hidden units. Default = 1
#' @param dim_hidden integer. Each hidden unit can take dim_hidden discrete values. Default = 2
#' @return Returns a list consisting of 5 initialised corex parameters:
#' \enumerate{
#'  \item{n_samples is the number of rows in the user supplied data.}
#'  \item{n_visible is the integer number of variables in user input data.}
#'  \item{alpha is a 2D adjacency matrix between input variables and hidden units. In range [0,1].}
#'  \item{p_y_given_x_3d is a 3D array of numerics in range (0, 1), that represent the probability that each observed x variable belongs to n_hidden latent variables of dimension dim_hidden. p_y_given_x_3d has dimensions (n_hidden, n_samples, dim_hidden).}
#'  \item{log_z is a 2D matrix containing the pointwise estimate of total correlation explained by each latent variable for each sample - this is used to estimate overall total correlation.}
#'  }
#' @keywords internal
#'
initialise_parameters <- function(data, n_hidden, dim_hidden ){
    # Get input data dimensions
    n_samples <- nrow(data)
    n_visible <- ncol(data)

    # Initialise alpha adjacency matrix
    if(n_hidden > 1){
        alpha = matrix(0.5 + 0.5*runif(n_hidden * n_visible*1, 0, 1), c(n_hidden, ncol=n_visible))
    } else {
        alpha = matrix(rep(1.0, n_hidden * n_visible), c(n_hidden, ncol=n_visible))
    }

    # Initialise unnormalised log post. probability
    #     temp <- gtools::rdirichlet(alpha= rep(1, dim_hidden), n= n_hidden*n_samples)
    temp <- gtoolsrdrich(alpha= rep(1, dim_hidden), n= n_hidden*n_samples)
    p_rand <- array( dim = c(n_hidden, n_samples, dim_hidden))
    for(i in 1: dim_hidden) {
        p_rand[ , , i] <- temp[, i]
    }

    # Normalise latent
    res1 <- normalise_latent(p_rand)
    p_y_given_x_3d <- res1$p_y_given_x_3d
    log_z <- res1$log_z

    # Export initialised parameters and data representations
    out <- list(n_samples = n_samples,
                n_visible = n_visible,
                alpha = alpha,
                p_y_given_x_3d = p_y_given_x_3d,
                log_z = log_z)
    return(out)
}



# helper function - copy of gtools::drichilet. Gtools package has become orphaned and is causing github actions failures for rcorex. Will restore use of gtools::rdrichilet if package is no longer orphaned
gtoolsrdrich <- function (n, alpha)
{
    l <- length(alpha)
    x <- matrix(rgamma(l * n, alpha), ncol = l, byrow = TRUE)
    sm <- x %*% rep(1, l)
    x/as.vector(sm)
}

