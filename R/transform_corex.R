#' @title transform_corex
#' @description Function to transform new_data to corex labels using fit corex object
#' @details Experimental!
#' @param corex_obj A fit biocorex object of class 'rcorex'.
#' @param new_data New data to transform into cluster labels.
#' @param details Boolean. Default is FALSE - returns cluster labels. If TRUE returns list of 2 arrays:
#' \enumerate{
#' \item{p_y_given_x_3d}
#' \item{log_z}
#' }
#' @export
transform_corex <- function(corex_obj, new_data, details = FALSE){

    # To do - data checks needed here to ensure new_data is compatible with original data and thus corex_obj

    log_marg_x_4d <- calculate_marginals_on_samples(new_data,
                            theta = corex_obj$theta,
                            marginal_description = corex_obj$marginal_description,
                            log_p_y = corex_obj$log_p_y,
                            dim_visible = NULL)

    latent <- calculate_latent(new_data,
                               alpha = corex_obj$alpha,
                               log_p_y = corex_obj$log_p_y,
                               log_marg_x_4d = log_marg_x_4d)

    p_y_given_x_3d_new <- latent$p_y_given_x_3d
    log_z_new <- latent$log_z

    labels <- t( apply(p_y_given_x_3d_new, c(1, 2), which.max) ) -1

    if (details == TRUE){
        out <- list( p_y_given_x_3d = p_y_given_x_3d_new,
                     log_z = log_z_new)
        return(out)
    } else {
        return(labels)
    }
}
