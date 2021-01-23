#' @title Fit biocorex to a dataset
#' @description Function which implements the CorEx algorithm for data with features of typical biomedical data such as continuous variables, missing data and under-sampled data.
#' @details
#' This function is a port of the original biocorex function in Python by Greg Ver Steeg: \url{https://github.com/gregversteeg/bio_corex}.
#' Reference: Greg Ver Steeg and Aram Galstyan. "Discovering Structure in High-Dimensional Data Through Correlation Explanation." NIPS, 2014. arXiv preprint arXiv:1406.1222.
#' @param data Data provided by user. For biocorex data can either be continuous (gaussian) or discrete (consectutive integers 0, 1, 2, 3...etc). Data types cannot by mixed in this implementation.
#' @param n_hidden An integer number of hidden variables to search for. Default = 1.
#' @param dim_hidden Each hidden unit can take \code{dim_hidden} discrete values. Default = 2
#' @param marginal_description Character string which determines the marginal distribution of the data. single marginal description applies to all variables in biocorex. Can be "gaussian" or "discrete". Default is "gaussian".
#' @param smooth_marginals Boolean (TRUE/FALSE) which indicates whether Bayesian smoothing of marginal estimates should be used.
#' @param eps The maximal change in TC across 10 iterations needed signal convergence
#' @param verbose Default FALSE. If TRUE, biocorex feeds back to user the iteration count and TCS each iteration. Useful to see progression if fitting a larger dataset.
#' @param repeats How many times to run biocorex on the data using random initial values. Corex will return the run which leads to the maximum TC. Default is 1. For a new dataset, recommend to leave it as 1 to see how long biocorex takes, however for more trustworthy results a higher numbers recommended (e.g. 25).
#' @param return_all_runs Default FALSE. If FALSE biocorex returns a single object of class rcorex. If TRUE biocorex returns all runs of biocorex as a list - the length of which = \code{repeats}. In this case the returned results are not rcorex objects, but have the same components of an rcorex object with class list.
#' @param max_iter numeric. Maximum number of iterations before ending. Default = 100
#' @return Returns either a rcorex object or a list of repeated runs as determined by the \code{return_all_runs} argument. An rcorex object is a list that contains the following components:
#' #' \enumerate{
#' \item{data - the user data supplied in call to corex.}
#' \item{call - the call used to run corex.}
#' \item{tcs - a vector of TC for n_hidden variables.}
#' \item{alpha - a 2D adjaceny matrix of connections between input variables and hidden variables.}
#' \item{p_y_given_x - a 3D array of numerics in range (0, 1), that represent the probability that each observed x variable belongs to n_hidden latent variables of dimension dim_hidden. p_y_given_x has dimensions (n_hidden, n_samples, dim_hidden).}
#' \item{theta - a list of the estimated parameters}
#' \item{log_p_y - a 2D matrix representing the log of the marginal probability of the latent variables.}
#' \item{log_z - a 2D matrix containing the pointwise estimate of total correlation explained by each latent variable for each sample - this is used to estimate overall total correlation.}
#' \item{dim_visible - only present if discrete marginals were specified. Lists the number of discrete levels that exist in the data.}
#' \item{iterations - the number of iterations for which the algorithm ran.}
#' \item{tc_history - a list that records the TC results for each iteration of the algorithm.}
#' \item{marginal_description - a character string which determines the marginal distribution of the data. }
#' \item{mis - an array that specifies the mutual information between each observed variable and hidden variable.}
#' \item{clusters - a vector that assigns a hidden variable label to each input variable.}
#' \item{labels - a 2D matrix of dimensions \code{(nrow(data), n_hidden)} that assigns a dimension label for each hidden variable to each row of data.}
#' }
#'
#' @import matrixStats tensor
#' @importFrom gtools rdirichlet
#' @export
#'
#'
biocorex <- function(data, n_hidden = 1, dim_hidden = 2, marginal_description = "gaussian",
                     smooth_marginals = FALSE, eps = 1e-6, verbose = FALSE,
                     repeats = 1, return_all_runs = FALSE, max_iter = 100){

    # Capture arguments for return to user in rcorex object
    cl <- match.call()

    # Detect factors and tell user to replace with dummy variables
    if (all(sapply(data, is.factor))){
        stop("Please convert factor variables to indicator variables.")
    }

    # Check all values are finite
    if ( any(sapply(data, is.infinite)) ){
        inf_cols <- (1:ncol(data)) [ apply(data, 2, function(x) any(is.infinite(x))) ]
        stop_msg <- paste0("Column(s): ",   paste(inf_cols, collapse = ", ") ,
                           " contain(s) infinities. Please remove infinities to use biocorex.\n")
        stop(stop_msg)
    }

    # Capture variable names for later use
    names <- names(data)

    # Loop over repeated runs of corex with different random initial values
    repeat_results <- lapply(1:repeats, function(m) {
        if( repeats > 1) {
            message(" Calculating repeat # ", m)
        } else {
            message("Calculating single iteration of corex")
        }

        # Initialise CorEx parameters
        inits <- initialise_parameters(data, n_hidden, dim_hidden )
        n_samples <- inits$n_samples
        n_visible <- inits$n_visible
        dim_visible <- NULL # only used when discrete marginal_description = "discrete"
        alpha <- inits$alpha
        p_y_given_x_3d <- inits$p_y_given_x_3d
        log_z <- inits$log_z
        if (marginal_description == "discrete"){
            values_in_data <- unique(sort(unlist(data))) # Get the set of unique values in the data
            values_in_data <- values_in_data[!is.na(values_in_data)] # remove NA if it is there
            dim_visible <- max(values_in_data) + 1
            if( ! all( seq(0, (dim_visible - 1)) == values_in_data) ) {
                warning("Data matrix values should be consecutive integers starting with 0,1,...")
                # consider to make this a stop error ?
            }
            if ( max(values_in_data >31 )){
                stop("To match Python version, discrete valued variables currently can take values from 0 to 31     only.") # May remove this
            }
        }

        # Initialise variable to track total correlation
        tc_history <- list()
        # Initialise variable to track correlation explained by each latent factor
        tcs = matrix(rep(0.0, n_hidden), nrow=n_hidden)
        tc_min = 0.01  # Try to "boost" hidden units with less than tc_min

        # Main loop to converge on optimal solution
        for(nloop in 1:max_iter){

            # Update Marginals
            log_p_y <- calculate_p_y(p_y_given_x_3d)
            theta <- calculate_theta(data, p_y_given_x_3d, marginal_description,
                                     smooth_marginals, dim_visible)
            log_marg_x_4d <- calculate_marginals_on_samples(data, theta, marginal_description,
                                                                log_p_y, dim_visible)

            # Structure learning step
            if (n_hidden > 1){
                alpha <- update_alpha(data, p_y_given_x_3d, tcs, tc_min, log_p_y, log_marg_x_4d)
            }

            latent <- calculate_latent(data, alpha, log_p_y, log_marg_x_4d)
            p_y_given_x_3d <- latent$p_y_given_x_3d
            log_z <- latent$log_z

            # Calculate TC and record history for convergence
            res_update <- update_tc(log_z, tcs, tc_history)
            tcs <- res_update$tcs
            tc_history <- res_update$tc_history
            if( verbose == TRUE){
                print(paste0("Iteration: ", nloop, ". TCS: ",
                             paste0(format(as.vector(tcs), digits = 3),
                                    collapse=" ")))
            }
            if( check_converged(tc_history, eps)==TRUE ) break
        }

        # Package results for return to user
        results <- sort_results(data, cl, n_hidden, dim_visible, marginal_description, smooth_marginals,
                                tcs, alpha, p_y_given_x_3d, theta,
                                log_p_y, log_z, tc_history, names)
        return(results)
    })

    # Prepare results for return to user
    if(return_all_runs == FALSE){
        out <- choose_maximal_run(repeat_results, repeats, max_iter)
        class(out) <- "rcorex"
    } else {
        out <- repeat_results
    }

    return(out)
}
