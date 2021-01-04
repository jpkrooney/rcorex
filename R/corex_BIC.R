#' @title corex_BIC
#' @description Experimental BIC calculation
#' @param corex_obj A fitted corex object
#'
#' @return numeric that contains BIC value
#' @export
#'
corex_BIC <- function(corex_obj){

    # Extract total correlation from corex object
    TC <- sum(corex_obj$tcs)
    n_hidden <- length(corex_obj$tcs)
    dim_hidden <- dim(corex_obj$p_y_given_x)[3]

    # calculate total entropy of raw data
    data <- corex_obj$data
    # for each variable calculate entropy and then add them up

    if( corex_obj$call$marginal_description == "gaussian"){
        ent_x <- lapply(1:dim(corex_obj$data)[2], function(i) {
            sd <- sd(corex_obj$data[, i], na.rm=TRUE)
            log(sd * sqrt(2*pi*exp(1) ) )
        })
        # parameter count k
        k <- 2* n_hidden * dim_hidden
    } else if( corex_obj$call$marginal_description == "discrete"){


        #k =(dim_data-1)*dim_hidden*n_variables_data
    }

    # Next calculate log likelihood via
    # log likelihood = TC - total entropy
    LL = TC - sum( unlist(ent_x) )

    # use likelihood and parameter count to calculate BIC = kln(n)-2*LL
    # n = nrow(data)
    BIC = k * log(nrow(data)) - 2*LL

    return(BIC)
}
