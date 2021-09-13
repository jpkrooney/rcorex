#


lc_update_ns <- function(data, moments, weights, eps, tol, yscale, verbose){
    # Perform one update of the weights and re-calculate moments in the NON-SYNERGISTIC case.

    m <- moments
    rj <- 1 - m$uj
    H <- (m$rhoinvrho / (1 + m$`Qi-Si^2`)) %*%  t(m$rhoinvrho)
    diag(H) <- 0
    grad <- weights / rj
    grad <- grad - 2 * m$invrho *m$rhoinvrho / (1 + m$Si)

    # Next line needs numerical recheck vs Python
    grad <- grad + m$invrho^2 * ((1 + m$rho^2) * m$Qij - 2*m$rho *m$Si) / (1 + m$`Qi-Si^2`)

    grad <- grad + H %*% weights
    sig_grad <- lc_sig(data, grad, eps)
    Bj <- rowSums(m$rho * grad)
    update_ <- - rj * (grad - 2 *weights / (2 - rj) * Bj) # Gamma Hess^-1 Grad

    #es_res <- matrix( integer(1) , nrow = nrow(sig_grad), ncol = ncol(sig_grad) )
    #for(j in 1:dim(sig_grad)[1]){
    #    for(i in 1:dim(sig_grad)[2]){
    #        es_res[j, i] <- es_res[j, i] + sig_grad[j, i] * update_[j,i]
    #    }
    #}
    update_tangent <- sum( sig_grad * update_ )

    if(update_tangent >=0){
        message("Note: covariance is nearly singular and this causes a loss of numerical precision. For this reason, we can no longer find an update that increases the objective. Hopefully this is a good solution. If not, this is caused by having many variables that are near duplicates. You could try again with the duplicates removed to look for other structure.'")
        return(list( weights, m))
    }
    backtrack <- TRUE
    eta <- 1

    while(backtrack == TRUE){
        #if(eta < min(tol, 1e-10)){
        if(eta < tol){
            if(verbose == TRUE){
                warning("Warning: step size becoming too small")
            }
            break
        }
        w_update <- weights + eta *update_
        m_update <- lc_calculate_moments_ns(data, w_update, quick = TRUE, eps, yscale)
        if(is.logical(m_update)){
            eta <- eta * 0.5
            if(verbose == TRUE){
                print(paste0("Eta = ", eta))
            }
            next
        }
        wolfe1 <- - m_update$TC <= - m$TC + 0.1 * eta * update_tangent
        if(is.na(wolfe1) | wolfe1 == FALSE){
            eta <- eta * 0.5
            if(verbose == TRUE){
                print(paste0("Wolfe1 = ", wolfe1, ". eta = ", eta))
            }
            next
        }
        backtrack <- FALSE
    }
    return( list(w_update = w_update, m_update = m_update ))
}




# Helper function
lc_sig <- function(data, u, eps){
    # Multiply the matrix u by the covariance matrix of x. We are interested in situations where n_variables >> n_samples, so we do this without explicitly constructing the covariance matrix."""

    y <- data %*% t(u)
    tmp_dot <- t(data) %*% y
    # Next, nv by m,  <X_i Y_j> / std Y_j
    prod <- (1 - eps^2) * t(tmp_dot) / nrow(data) + eps^2 * u
    return( prod )
}


