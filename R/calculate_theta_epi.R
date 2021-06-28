#' @title calculate_theta_epi
#' @description Internal function to calculate parameters for each dimension of each hidden variable for the epicorex function
#' @details
#' This function is calls functions \code{\link{estimate_parameters_gaussian}} or \code{\link{estimate_parameters_discrete}} depending on the \code{marginal_description}.
#' @param data Data provided by user
#' @param p_y_given_x_3d A 3D array of numerics in range (0, 1), that represent the probability that each observed x variable belongs to n_hidden latent variables of dimension dim_hidden. p_y_given_x_3d has dimensions (n_hidden, n_samples, dim_hidden).
#' @param marginal_description Character string which determines the marginal distribution of the data. single marginal description applies to all variables in biocorex
#' @param smooth_marginals Boolean (TRUE/FALSE) which indicates whether Bayesian smoothing of marginal estimates should be used.
#' @return Returns a list of estimated parameters. The list has length = number of columns in the supplied \code{data}. The elements of the returned list depend on the marginal_description as follows: \itemize{ \item If the marginal description is "gaussian" a list of 2 arrays the first of which represent the estimate means, the second the estimated standard deviations. Each of these lists will have dimensions (n_hidden, dim_hidden).\item If the marginal description is "discrete", each element contains a list of length...}
#'
#' @keywords internal
#'
calculate_theta_epi <- function(data, p_y_given_x_3d, marginal_description,
                            smooth_marginals){

    n_visible <- dim(data)[2]


    theta <- lapply(1:n_visible, function(i) {

        x_i <- data[, i]
        not_missing <- !is.na(x_i)

        if(marginal_description[i] == "gaussian"){
            estimate_parameters_gaussian(x_i[not_missing],
                                         p_y_given_x_3d[, not_missing, , drop = FALSE],
                                         smooth_marginals)
        #} else if(marginal_description[i] == "discrete") {
        #    estimate_parameters_discrete(x_i[not_missing],
        #                                 p_y_given_x_3d[, not_missing, , drop = FALSE],
        #                                 smooth_marginals, dim_visible)
        } else if(marginal_description[i] == "bernoulli") {
            estimate_parameters_bernoulli(x_i[not_missing],
                                         p_y_given_x_3d[, not_missing, , drop = FALSE],
                                         smooth_marginals)
        } else {
            stop(paste0("The marginal description, ", marginal_description[i], " is not implemented."))
        }
    } )

    return(theta)
}

