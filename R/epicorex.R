#' @title Fit epicorex to a dataset
#' @description Function which implements the CorEx algorithm for data with features of typical biomedical data such as continuous variables, missing data and under-sampled data, and extended to accommodate mixed marginal descriptions (i.e. mixed data types)
#' @details
#' This function is an extension of the biocorex function to allow for mixed data-types as typically found in epidemiological datasets - e.g. categorical data + continuous data for example.
#' @param data Data provided by user. Allows for mixed data types. Eg categorical or binomial or continuous or discrete in same dataset
#' @param n_hidden An integer number of hidden variables to search for. Default = 1.
#' @param dim_hidden Each hidden unit can take \code{dim_hidden} discrete values. Default = 2
#' @param marginal_description Character string which determines the marginal distribution of the data. For epicorex, marginal_description must be a vector of strings of length equal to the number of columns in \code{data}. Allowable marginal descriptions are: gaussian, discrete, bernoulli currently - more may be added later.
#' @param smooth_marginals Boolean (TRUE/FALSE) which indicates whether Bayesian smoothing of marginal estimates should be used.
#' @param eps The maximal change in TC across 10 iterations needed signal convergence
#' @param verbose Default FALSE. If TRUE, epicorex feeds back to user the iteration count and TCS each iteration. Useful to see progression if fitting a larger dataset.
#' @param repeats How many times to run epicorex on the data using random initial values. Corex will return the run which leads to the maximum TC. Default is 1. For a new dataset, recommend to leave it as 1 to see how long epicorex takes, however for more trustworthy results a higher numbers recommended (e.g. 25).
#' @param return_all_runs Default FALSE. If FALSE epicorex returns a single object of class rcorex. If TRUE epicorex returns all runs of epicorex as a list - the length of which = \code{repeats}. In this case the returned results are not rcorex objects, but have the same components of an rcorex object with class list.
#' @param max_iter numeric. Maximum number of iterations before ending. Default = 100
#' @param negcheck_iter numeric. Number of iterations at which to check for persistent negative total tcs. IF detected the corex run stops. The default is 30.
#' @param neg_limit numeric. At the negcheck_iter number of iterations, all prior iterations are checked versus this value. If all values are below this value, persistent negative tcs is declared. This is an indication that some data is not well described by the marginal description used. The default is -1.
#' @param logpx_method EXPERIMENTAL - A character string that controls the method used to calculate log_p_xi. If "pycorex" uses the same method as the Python version of biocorex, if set to "mean" calculates an estimate of log_p_xi by averaging across n_hidden estimates. NOTE, that mean may become the default option after further testing.
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
#' @import matrixStats
#' @importFrom gtools rdirichlet
#' @export
#'
#'
epicorex <- function(data, n_hidden = 1, dim_hidden = 2, marginal_description,
                     smooth_marginals = FALSE, eps = 1e-6, verbose = FALSE,
                     repeats = 1, return_all_runs = FALSE, max_iter = 100,
                     negcheck_iter = 30, neg_limit = -1,
                     logpx_method = "pycorex"){

    # Capture arguments for return to user in rcorex object
    cl <- match.call()

    # To do - input data checks and prep
    # Detect factors...sapply(data, is.factor)
    if (all(sapply(data, is.factor))){
        stop("Please convert factor variables to indicator variables. The indicator variables should be given the marginal description 'bernoulli'.")
    }

    # Check all values are finite
    if ( any(sapply(data, is.infinite)) ){
        inf_cols <- (1:ncol(data)) [ apply(data, 2, function(x) any(is.infinite(x))) ]
        stop_msg <- paste0("Column(s): ",   paste(inf_cols, collapse = ", ") ,
                           " contain(s) infinities. Please remove infinities to use epicorex.\n")
        stop(stop_msg)
    }

    # Capture variable names for later use
    names <- names(data)


    # For columns mathcing marginals = discrete, calculate dim_visible of the column
    dim_visible <- rep(NA, length(marginal_description))
    for(i in 1:length(marginal_description)){
        if (marginal_description[i] == "discrete"){
            dim_visible[i] <- get_dimvisible(data[, i])
        }
    }


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
        alpha <- inits$alpha
        p_y_given_x_3d <- inits$p_y_given_x_3d
        log_z <- inits$log_z
        state <- NULL # variable to hold final state of corex - i.e. converged, not-converged or persistent negative tcs detected

        # Initialise variable to track total correlation
        tc_history <- list()
        # Initialise variable to track correlation explained by each latent factor
        tcs = matrix(rep(0.0, n_hidden), nrow=n_hidden)
        tc_min = 0.01  # Try to "boost" hidden units with less than tc_min

        # Main loop to converge on optimal solution
        for(nloop in 1:max_iter){

            # Update Marginals
            log_p_y <- calculate_p_y(p_y_given_x_3d)
            theta <- calculate_theta_epi(data, p_y_given_x_3d, marginal_description,
                                     smooth_marginals, dimvis_byvars = dim_visible)
            log_marg_x_4d <- calculate_marginals_on_samples(data, theta, marginal_description,
                                                            log_p_y, returnratio = TRUE,
                                                            logpx_method = logpx_method)

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
            if( check_converged(tc_history, eps)==TRUE ){
                state <- "Converged"
                if( sum(tcs) < 0 ){
                    state <- "Negative tcs"
                }
                break
            }

            # Detect persistent negative tcs and break out of loop if detected
            if ( nloop == negcheck_iter ){
                if( all( sapply(tc_history, sum)[10:negcheck_iter] < neg_limit) ){
                    state <- "Persistent negative tcs"
                    break
                }
            }
            if ( nloop == max_iter ){
                state <- "Unconverged"
            }
        }

        # Package results for return to user
        results <- sort_results(data, cl, n_hidden, dim_visible = dim_visible, marginal_description,
                                smooth_marginals, tcs, alpha, p_y_given_x_3d,
                                theta, log_p_y, log_z, tc_history, names, state,
                                logpx_method = logpx_method)
        return(results)
        #repeat_results[[m]] <- results
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
