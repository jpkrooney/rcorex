#' @title corex_RMSE
#' @description Experimental root-mean-squared-error calculation
#' @param corex_obj A fitted corex object
#' @param new_data Optionally provided new data. Must contain same variables as fit data. If new_data not provided uses fit data
#'
#' @return numeric that represents the RMSE
#' @export
#'
corex_RMSE <- function(corex_obj, new_data = NULL){

    # To do - some data checks needed here to ensure new_data is compatible
    # with original data and therefore corex_obj

    # if new data not provided - use fit data
    if(is.null(new_data)){
        new_data <- corex_obj$data
    }

    # Get marginals
    log_marg_x_4d <- calculate_marginals_on_samples(data = new_data,
                            theta = corex_obj$theta,
                            marginal_description = corex_obj$call$marginal_description,
                            log_p_y = corex_obj$log_p_y,
                            dim_visible = corex_obj$dim_visible)

    # Calculation RMS of log_marg_x_4d
    RMSE <- mean ( log_marg_x_4d^2 )^0.5
    return(RMSE)
}
