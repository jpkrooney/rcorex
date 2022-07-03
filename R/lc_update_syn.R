#' Perform one update of the weights and re-calculate moments in the SYNERGISTIC case
#'
#'@keywords internal

lc_update_syn <- function(data, moments, weights, eps, tol, yscale, verbose, eta = 0.5){
    m <- moments

    H <- t(1 / m$`X_i^2 | Y` * m$X_iZ_j) %*% m$X_iZ_j
    diag(H) <- 0
    R <- t(m$X_iZ_j) / m$`X_i^2 | Y`

    S <- H %*% weights

    w_update <- (1 - eta) * weights + eta * (R -S)
    m_update <- lc_calculate_moments_syn(data, weights, eps, yscale)

    return( list(w_update = w_update, m_update = m_update ))
}


