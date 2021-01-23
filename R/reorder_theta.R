#' @title reorder_theta
#' @description Internal helper function to reorder the theta parameters list
#' @details This function is necessary as the structure of theta varies for each parameter type. THis function is used after convergence to help package results for return to user. Is it called by \code{\link{sort_results}} which passes the desired order via the \code{ord} parameter.
#' @param theta List of estimated parameters.
#' @param marginal_description Character string which determines the marginal distribution of the data. single marginal description applies to all variables in biocorex
#' @param ord Vector of integers that give desired reorder sequence
#' @return Returns a new version of theta reordered according to the pattern supplied to \code{ord}
#' @keywords internal
#'
reorder_theta <- function(theta, marginal_description, ord){

    for( i in 1:length(theta)){
        if(marginal_description == "discrete"){

            theta[[i]] <- theta[[i]][ , ord, , drop = FALSE]

        } else if(marginal_description == "gaussian"){

            theta[[i]]$mean_ml <- theta[[i]]$mean_ml[ord, , drop = FALSE]
            theta[[i]]$sig_ml <- theta[[i]]$sig_ml[ord, , drop = FALSE]
        }
    }
    return(theta)
}
