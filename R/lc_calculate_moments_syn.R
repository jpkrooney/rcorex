#' Calculate moments based on the weights and samples. We also calculate and save MI, TC, additivity, and the value of the objective. Note it is assumed that <X_i^2> = 1!
#'
#'
#'@keywords internal

lc_calculate_moments_syn <- function(data, weights, eps, yscale){
    m <- list()

    y <- data %*% t(weights) # + noise, but it is included analytically

    m$'X_iY_j' <- t(data) %*% y / nrow(data)

    m$cy <- weights %*% m$'X_iY_j' + yscale^2 * diag( nrow(weights))
    m$'Y_j^2' <- as.matrix( diag(m$cy) )
    m$ry <- m$cy / (sqrt(m$'Y_j^2') %*% sqrt( t(m$'Y_j^2') ))
    m$rho <- t( m$X_iY_j / c( sqrt(m$`Y_j^2`) ) )
    m$invrho <- 1 / (1 - m$rho^2)
    m$rhoinvrho <- m$rho * m$invrho
    m$Qij <- m$ry %*% m$rhoinvrho
    m$Qi <-colSums(m$rhoinvrho * m$Qij )
    m$Si <- colSums(m$rho * m$rhoinvrho)

    m$MI <- -0.5 * log(1 - m$rho^2)
    m$'X_iZ_j' <- t(solve(m$cy, t(m$X_iY_j)))


    #einsum calculation
    es_res <- rep(0, dim(m$'X_iZ_j')[1])
    for(i in 1:dim(m$'X_iZ_j')[1]){
        for(j in 1:dim(m$'X_iZ_j')[2]){
            es_res[i] <- es_res[i] + m$X_iZ_j[i,j] * m$'X_iY_j'[i,j]
        }
    }
    es_res[es_res <- 1e-6] <- 1e-6
    m$'X_i^2 | Y' <- 1 - es_res

    mi_yj_x <- 0.5 * log(m$`Y_j^2`) - 0.5 * log(yscale^2)
    mi_xi_y <- -0.5 * log(m$`X_i^2 | Y`)

    m$TCs <- rowSums(m$MI) - mi_yj_x
    m$additivity <- sum( colSums( m$MI ) - mi_xi_y )
    m$TC <- sum(mi_xi_y) - sum(mi_yj_x)

    return(m)
}
