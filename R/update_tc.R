#' @title Internal function to calculate marginal probility of latent variables
#' @param log_z A 2D matrix containing the pointwise estimate of total correlation explained by each latent variable for each sample.
#' @param tcs The total correlation explained by each latent variable in unit of nats (natural unit of information)
#' @param tc_history A log of the estimated total correlation with each iteration of the algorithm.
#' @return log_p_y. The log of the marginal probability of the latent variables.
#' @keywords internal
#'
update_tc <- function(log_z, tcs, tc_history){

    tcs <- matrix(unlist(lapply(1:dim(log_z)[1], function(x) mean(log_z[x, ]) )), ncol=1)

    len <- length(tc_history)
    tc_history[[len+1]] <- tcs
    return(list(tcs=tcs, tc_history=tc_history))
}
