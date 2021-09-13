#    """Transform each marginal to be as close to a standard Gaussian as possible.
#        'standard' (default) just subtracts the mean and scales by the std.
#        'empirical' does an empirical gaussianization (but this cannot be inverted).
#        'outliers' tries to squeeze in the outliers
#        Any other choice will skip the transformation."""


lc_preprocess <- function(data, fit = FALSE, gaussianize, theta = NULL){

    if( any(is.na(data)) ){
        # impute stuff here
        res <- lc_mean_impute(data)
        data <- res$data
        n_obs <- n_obs
    } else {
        n_obs <- nrow(data)
    }

    if (fit == TRUE){
        if(gaussianize == "none"){
            # no action
        } else if(gaussianize == "standard"){
            means <- colMeans(data)
            stds <- sqrt((rowSums( (t(data) - means )^2) / n_obs))
            stds[ stds < 1e-10] <- 1e-10 # minimum
            theta <- list(means = means, stds = stds)
            data2 <- (data - rep(means, each=n_obs)) / rep(stds, each = n_obs)
            if(max(abs(data2)) > 6){
                message("Note: outliers more than 6 stds away from mean detected. Consider using gaussianize='outliers'")
            }
        } else if (gaussianize == "outliers"){
            means <- colMeans(data)
            stds <- sqrt((rowSums( (t(data) - means )^2) / nrow(data)))
            stds[ stds < 1e-10] <- 1e-10 # minimum
            theta <- list(means = means, stds = stds)
            data2 <- g( (data - rep(means, each=n_obs)) / rep(stds, each = n_obs) )
        } else if (gaussianize == "empirical"){
            warning("Warning: correct inversion/transform of empirical gauss transform not implemented.")
            data2 <- sapply(1:ncol(data),
                            function(i)
                                qnorm( (rank(data[,i]) - 0.5) / length(data[,i])) )
        }
    } else if(fit == FALSE) {
        means <- theta$means
        stds <- theta$stds
        if(gaussianize == "none"){
            # no action
        } else if(gaussianize == "standard"){
            data2 <- (data - rep(means, each=n_obs)) / rep(stds, each = n_obs)
            if(max(abs(data2)) > 6){
                message("Note: outliers more than 6 stds away from mean detected. Consider using gaussianize='outliers'")
            }
        } else if (gaussianize == "outliers"){
            data2 <- g( (data - rep(means, each=n_obs)) / rep(stds, each = n_obs) )
        } else if (gaussianize == "empirical"){
            warning("Warning: correct inversion/transform of empirical gauss transform not implemented.")
            data2 <- sapply(1:ncol(data),
                            function(i)
                                qnorm( (rank(data[,i]) - 0.5) / length(data[,i])) )
        }
    }

    return(list(data = data2,
                theta = theta))
}


# Helper function g
g <- function(data, th = 4){
    #    """A transformation that suppresses outliers for a standard normal."""
    data2 <- data
    data2[data2 < -th] <- -th
    data2[data2 > th] <- th
    diff <- tanh(data - data2)
    return(data2 + diff)
}




