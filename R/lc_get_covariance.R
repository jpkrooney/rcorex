#' This uses E(Xi|Y) formula for non-synergistic relationships
#'
#'


lc_get_covariance <- function(lincor_obj){
    m <- lincor_obj$moments
    discourage_overlap <- lincor_obj$discourage_overlap
    eps <- lincor_obj$eps
    means <- as.matrix( lincor_obj$theta$means )

    if (discourage_overlap == TRUE){
        z <- m$rhoinvrho / (1 + m$Si)
        tmp <- t(z) %*% z
        tmp <- tmp / (1 - eps^2)
        diag(tmp) <- 1
        cov <- c(means) %*% t(means) *tmp
    } else {
        #einsum calculation
        es_res <- matrix( integer(1),
                          nrow = dim(m$X_iZ_j)[1],
                          ncol = dim(m$X_iY_j)[1])
        for(i in 1:dim(m$X_iZ_j)[1]){
            for(j in 1:dim(m$X_iZ_j)[2]){
                for(k in 1:dim(m$X_iY_j)[1] )
                es_res[i, k] <- es_res[i, k] + m$X_iZ_j[i, j] * m$X_iZ_j[k, j]
            }
        }
        diag(es_res) <- 1
        cov <- c(means) %*% t(means) *es_res
    }

    return(cov)
}

