


lc_predict <- function(lincor_obj, data){
    # Extract needed fit parameters
    gaussianize <- lincor_obj$gaussianize
    theta <- lincor_obj$theta

    # transform data
    z <- lc_transform(data, details = FALSE, lincor_obj)

    # np.dot(self.moments["X_i Z_j"], y.T)
    y <- t(lincor_obj$moments$X_iZ_j %*% t(as.matrix(z$labels)))

    if(gaussianize == "standard"){
        result <- theta$stds * y + theta$means
        #return self.theta[1] * x + self.theta[0]
    } else if(gaussianize == "outliers"){
        result <- theta$stds * g_inv(y) + theta$means
        #return self.theta[1] * g_inv(x) + self.theta[0]
    }
    return(result)
}



# Helper function
g_inv <- function(x, t = 4){
    # Inverse of g transform

    #def g_inv(x, t=4):
    #    """Inverse of g transform."""
    #xp = np.clip(x, -t, t)
    #diff = np.arctanh(np.clip(x - xp, -1 + 1e-10, 1 - 1e-10))
    #return xp + diff
    xp <- x
    xp[xp < -t] <- -t
    xp[xp > t] <- t

    diff <- x - xp
    diff[diff < (-1 + 1e-10)] <- -1 + 1e-10
    diff[diff > (1 - 1e-10)] <- 1 - 1e-10

    diff <- atanh(diff)

    return(xp + diff)
}
