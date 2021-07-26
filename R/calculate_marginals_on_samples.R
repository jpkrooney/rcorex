#' @title Calculate marginals on data
#' @description Internal function to calculate marginals given data and parameter estimates
#' @details
#'Calculate the value of the marginal distribution for each variable, for each dimension of each hidden variable and each sample
#' @param data Data provided by user
#' @param theta List of estimated parameters
#' @param marginal_description Character string which determines the marginal distribution of the data. A single marginal description applies to all variables in biocorex.
#' @param log_p_y A 2D matrix representing the log of the marginal probability of the latent variables
#' @param dim_visible The dimension of the data provided - i.e. the number of discrete levels that exist in the data. Must be positive integer.
#' @param returnratio A Boolean that returns log_marg_x_4d for the value TRUE and log_p_xi_given_y_4d for the value FALSE. Intended for development use and may not be retained long term.
#' @param logpx_method EXPERIMENTAL - A character string that controls the method used to calculate log_p_xi. "default" uses the same method as the Python version of biocorex, "mean" calculates an estimate of log_p_xi by averaging across y estimates.
#' @return 4D array of dimensions: \code{(n_hidden, n_samples, n_visible, dim_hidden )} where n_samples is the number of rows in the user provided data, and n_visible is the number of columns. Returned data is result fo the calculation \eqn{log \left( \frac{p\left(y_{j} \mid x_{i}\right)}{p\left(y_{j}\right)} \right))} for each j,sample,i,y_j
#'
#'@keywords internal
#'
calculate_marginals_on_samples <- function(data, theta, marginal_description,
                                           log_p_y,  dim_visible=NULL, returnratio = TRUE,
                                           logpx_method = "default"){
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
    # Pack marginals, log p(xi|y), into 4D array
    log_p_xi_given_y_4d <- aperm( array( unlist( calcs_results ),
                                   dim = c(n_hidden, dim_hidden, n_samples, n_visible) ),
                            c(1, 3, 4, 2))

    if(returnratio == FALSE){
        return( log_p_xi_given_y_4d )
    } else {
        # Broadcast log_p_y to 4D and permute as preparatory step for ratio calculation
        log_p_y_4d <- aperm( array( log_p_y, dim = c( dim(log_p_y), n_visible,  n_samples)),
                             c(1, 4, 3, 2))

        #### Calculate ratio ####
        # Aim is to calculate log ( p(xi|y)/p(xi) ) - or log (p(y|xi)/p(y)) by Bayes rule
        # Approach is to calculate log p(y|xi) = log p(xi,y) - log p(x)
        # where p(x) = sum p(xi,y) over all y

        # calculate log p(xi,y) = log p(xi|y) + log p(y)
        log_joint_pxi_y <- log_p_xi_given_y_4d + log_p_y_4d

        # Calculate log p(y|xi) = log p(xi,y) - logsumexp log p(xi,y)
        log_p_x <- logSumExp4D( log_joint_pxi_y )
        if (logpx_method == "mean"){
            temp <- log_p_x
            dim(temp) <- c(n_hidden, n_visible * n_samples)
            log_p_x <- colMeans(temp)
            dim(log_p_x) <- c(n_samples, n_visible)
        } else if ( !logpx_method %in% c("default", "mean")){
            stop("Invalid logpx_method specified.")
        }
        log_p_y_given_xi <- log_joint_pxi_y - c( log_p_x )

        # Finally, calculate log ( p(y|xi)/p(y) ) = logp(y|xi) - log(p_y)
        log_marg_x_4d <- log_p_y_given_xi - log_p_y_4d

        return( log_marg_x_4d )
    }
}

# Helper function
logSumExp4D <- function( log_joint_pxi_y ) {
    dims <- dim( log_joint_pxi_y )[1:3]
    dropdim <- dim( log_joint_pxi_y )[4]
    len_res <- prod( dims )   # length of results object
    dim( log_joint_pxi_y ) <- c( len_res, dropdim ) # reshape data from 4D to 2D
    rmaxs <- rowMaxs( log_joint_pxi_y ) # get max of each row
    res <- rmaxs + log(rowSums( exp( ( log_joint_pxi_y ) - rmaxs ) ) ) # perform logsumexp calc
    res <- array(res, dim = dims) #reshape for returning to calling function
    return(res)
}
