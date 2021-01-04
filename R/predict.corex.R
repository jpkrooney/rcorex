#' @importFrom stats rnorm
# experimental
predict.corex <- function(corex_obj, new_data=NULL){

    # if new_data provided must first transform new_data into corex representation
    if(!is.null(new_data)){
        new_labels <- transform_corex(corex_obj, new_data )
    } else {
        new_labels <- corex_obj$labels
    }

    ### should be getting new clusters from transform also ?


    # Get dims of data
    pred_dims <- if( !is.null(new_data) ) {
        dim(new_data)
    } else {
        dim(corex_obj$data)
    }

    # using new data and corex model calculate marginals
    log_marg_x_4d <- calculate_marginals_on_samples(data = new_data,
                                        theta = corex_obj$theta,
                                        marginal_description = corex_obj$call$marginal_description,
                                        log_p_y = corex_obj$log_p_y,
                                        dim_visible = corex_obj$dim_visible)

    # Make a matrix to hold predictions
    pred_data <- matrix( nrow = pred_dims[1], ncol = pred_dims[2] )

    # Predict new data dependent on clusters + labels + thetas for each element of pred_data
    for( i in 1:nrow(pred_data)){
        for (j in 1:ncol(pred_data)){
            clust <- corex_obj$clusters[j]
            lab <- new_labels[i, clust + 1]

            means <- corex_obj$theta[[ j ]]$mean_ml
            sigs <- corex_obj$theta[[ j ]]$sig_ml

            pred_data[i, j] <- rnorm(1,
                                     mean = means[clust + 1, lab + 1],
                                     sd = sigs[clust + 1, lab + 1])
        }
    }
    pred_data <- data.frame(pred_data)
    names(pred_data) <- names(corex_obj$clusters)


    # From marginals - back calculate to predict data - need reverse marginals fns











    return(pred_data)
}
