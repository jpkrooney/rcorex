#' @title Calculate marginals on data
#' @description Internal function to calculate marginals given data and parameter estimates
#' @details
#'Calculate the value of the marginal distribution for each variable, for each dimension of each hidden variable and each sample
#' @param data Data provided by user
#' @param theta List of estimated parameters
#' @param marginal_description Character string which determines the marginal distribution of the data. A single marginal description applies to all variables in biocorex.
#' @param log_p_y A 2D matrix representing the log of the marginal probability of the latent variables
#' @param minmarg EXPERIMENTAL parameter. If NULL has no effect. If not NULL should be a negative number that places a minimal value on the log_marginal value.
#' @param dim_visible The dimension of the data provided - i.e. the number of discrete levels that exist in the data. Must be positive integer.
#'
#' @return 4D array of dimensions: \code{(n_hidden, n_samples, n_visible, dim_hidden )} where n_samples is the number of rows in the user provided data, and n_visible is the number of columns. Returned data is result fo the calculation \eqn{log \left( \frac{p\left(y_{j} \mid x_{i}\right)}{p\left(y_{j}\right)} \right))} for each j,sample,i,y_j
#'
#'@keywords internal
#'
calculate_marginals_on_samples <- function(data, theta, marginal_description,
                                           minmarg = NULL, log_p_y,  dim_visible=NULL){
    # Get data and parameter dimensions
    n_hidden <- dim(log_p_y)[1]
    dim_hidden <- dim(log_p_y)[2]
    n_samples <- dim(data)[1]
    n_visible <- dim(data)[2]

    # Calculate the marginals
    if (length(marginal_description) ==1){
        calcs_results <- lapply(1:n_visible, function(i)
            calculate_p_xi_given_y(data[,i], theta[[i]], marginal_description, dim_visible) )
    } else if (length(marginal_description) == n_visible){
        calcs_results <- lapply(1:n_visible, function(i)
            calculate_p_xi_given_y(data[,i], theta[[i]], marginal_description[i]) )
    } else {
        stop("Invalid marginals (from fn calculate_marginals_on_samples()")
    }

    # Pack results into 4D array
    #log_marg_x_4d <- array( unlist( calcs_results ),
    #                  dim = c(n_hidden, n_samples, dim_hidden, n_visible) )
    #log_marg_x_4d <- aperm(log_marg_x_4d, c(1, 2, 4, 3))
    log_marg_x_4d <- array( unlist( calcs_results ),
                            dim = c(n_hidden, dim_hidden, n_samples, n_visible) )
    log_marg_x_4d <- aperm(log_marg_x_4d, c(1, 3, 4, 2))

    # to add and subtract with log_p_y need to broadcast in permuted form across extra dimensions
    log_p_y_4d <- aperm( array( rep(log_p_y, n_samples * n_visible),
                                dim = c( dim(log_p_y), n_visible,  n_samples)),
                         c(1, 4, 3, 2))

    # Now add 4D form of log_p_y to log marginals
    log_marg_x_4d <- log_marg_x_4d + log_p_y_4d

    # Calculate logSumExp term
    term <- logSumExp4D(log_marg_x_4d)

    # Subtract logSumExp from marginals
    log_marg_x_4d <- log_marg_x_4d - c(term)

    # Finally subtract log_p_y_4d again
    log_marg_x_4d <- log_marg_x_4d - log_p_y_4d

    # Apply minimum value on log_marg_x_4d
    if( !is.null(minmarg) ){
        log_marg_x_4d[log_marg_x_4d < minmarg] <- minmarg
    }

    return(log_marg_x_4d)
}

# Helper function
logSumExp4D <- function (data) {
    dims <- dim(data)[1:3]
    dropdim <- dim(data)[4]
    len_res <- prod( dims )   # length of results object
    dim( data ) <- c( len_res, dropdim ) # reshape data from 4D to 2D
    rmaxs <- rowMaxs(data) # get max of each row
    res <- rmaxs + log(rowSums( exp( (data) - rmaxs ) ) ) # perform logsumexp calc
    res <- array(res, dim = dims) #reshape for returning to calling function
    return(res)
}
