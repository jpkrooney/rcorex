#' @title sort_results
#' @description Internal function to sort biocorex results for output to user.
#' @param data Data provided by user
#' @param cl User call to biocorex
#' @param n_hidden An integer number of hidden variables to search for.
#' @param dim_visible The dimension of the data provided when discrete marginal distribution is specified - i.e. the number of discrete levels that exist in the data. Must be positive integer.
#' @param marginal_description Character string which determines the marginal distribution of the data. single marginal description applies to all variables in biocorex.
#' @param smooth_marginals Boolean (TRUE/FALSE) which indicates whether Bayesian smoothing of marginal estimates should be used.
#' @param tcs Vector of length n_hidden - contains the TC for each hidden factor. This is used to decided the sort order for all the other parameters such that hidden factors are returned to used in order of largest TC to smallest TC.
#' @param alpha Adjacency matrix between input variables and hidden variables. In range [0,1].
#' @param p_y_given_x_3d A 3D array of numerics in range (0, 1), that represent the probability of n_hidden latent variables of dimension dim_hidden, for each observed x variable with dimensions (n_hidden, n_samples, dim_hidden)
#' @param theta List of estimated parameters
#' @param log_p_y A 2D matrix representing the log of the marginal probability of the latent variables.
#' @param log_z A 2D matrix containing the pointwise estimate of total correlation explained by each latent variable for each sample - this is used to estimate overall total correlation.
#' @param tc_history A list that records the TC results for each iteration of the algorithm. Used to calculate if convergence has been reached.
#' @param names A vector of the variables names of the supplied data.
#' @param state A string that describes the final state of corex (i.e. "Converged", "Negative tcs", "Unconverged").
#' @param logpx_method EXPERIMENTAL - A character string that controls the method used to calculate log_p_xi. "pycorex" uses the same method as the Python version of biocorex, "mean" calculates an estimate of log_p_xi by averaging across n_hidden estimates.
#' @keywords internal
#' @return Returns list of corex algorthim results sorted in descending order by TC of the latent variables. The list includes the following elements:
#' \enumerate{
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
sort_results <- function(data, cl, n_hidden, dim_visible, marginal_description, smooth_marginals,
                         tcs, alpha, p_y_given_x_3d, theta,
                         log_p_y, log_z, tc_history, names, state,
                         logpx_method){

    # Order components from strongest TC to weakest
    ord <- order(tcs, decreasing=TRUE)
    # Pack results into a list with list elements sorted by ord
    results <- list( data = data,
                     call = cl,
                     tcs = tcs[ord],
                     alpha = alpha[ord,],
                     p_y_given_x = p_y_given_x_3d[ord, , , drop = FALSE],
                     theta = if(length(marginal_description) ==1){
                         reorder_theta(theta, marginal_description, ord)
                     } else {
                         reorder_theta_epi(theta, marginal_description, ord)
                     },
                     log_p_y = if(n_hidden ==1 ) { log_p_y} else {log_p_y[ord, ] },
                     log_z = log_z[ord,],
                     dim_visible = if(length(marginal_description)==1) {
                         if(marginal_description == "discrete"){
                             dim_visible
                         }
                     } else { NULL },
                     iterations = length(tc_history),
                     tc_history = tc_history,
                     marginal_description = marginal_description,
                     state = state)

    # Add mutual information (mis) to results
    results$mis <- calculate_mis(data = data, theta = results$theta,
                                 marginal_description = marginal_description,
                                 log_p_y = results$log_p_y,
                                 p_y_given_x_3d = results$p_y_given_x,
                                 dim_visible = dim_visible,
                                 logpx_method = logpx_method)

    # Adjust shape of alpha if n_hidden ==1
    if(n_hidden==1){ results$alpha <- t(matrix(results$alpha)) }

    # Apply bootstrap of mis - moved here from biocorex() to avoid running this on discarded answers
    boot_res <- mi_bootstrap(data, marginal_description,
                             results$theta, results$log_p_y, results$p_y_given_x,
                             dim_visible, smooth_marginals, logpx_method = logpx_method)
    results$mis <- (results$mis - boot_res$bias) * (results$mis > boot_res$sig)

    # Assign clusters to result variables (columns)
    results$clusters <- unlist( lapply(1:dim(results$alpha)[2],
                                       function(x) which.max(results$alpha[, x ] ) ) ) -1

    # Assign variable names to clusters
    names(results$clusters) <- names

    # Assign labels to result samples (rows)
    results$labels <- t( apply(results$p_y_given_x, c(1, 2), which.max) ) -1

    return(results)
}
