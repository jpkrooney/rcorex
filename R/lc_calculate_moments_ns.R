# Calculate moments based on the weights and samples. We also calculate and save MI, TC, additivity, and
#        the value of the objective. Note it is assumed that <X_i^2> = 1! """


# test weights from python - move to test file later
data = matrix(c(1,0,0,0,0,
                0,0,0,1,1,
                1,1,1,0,0,
                1,1,1,1,1), ncol=5, byrow = TRUE)

weights <- matrix(c(-0.   ,  0.016, -0.034,  0.042,  0.097,
                0.006, -0.018, -0.062,  0.026, -0.095,
                0.038, -0.01 , -0.051,  0.067,  0.067,
                0.076, -0.101, -0.045,  0.009,  0.056,
               -0.001,  0.129, -0.009, -0.04 , -0.006,
               -0.005, -0.077, -0.067,  0.045,  0.007,
               -0.013, -0.071, -0.045, -0.032,  0.   ,
               -0.045,  0.049, -0.104,  0.066,  0.006,
                0.029,  0.092,  0.006,  0.028, -0.079,
                0.03 ,  0.089, -0.027,  0.05 ,  0.008), nrow = 10, byrow = TRUE)


#library(JuliaCall)
#julia_command("import RCall.rcopytype; rcopytype(::Type{RCall.RClass{:JuliaFloat32}}, x::Ptr{RealSxp}) = Array{Float32}")



lc_calculate_moments_ns <- function(data, weights, quick, eps, yscale){
    m <- list()

    y <- data %*% t(weights) # + noise, but it is included analytically
    tmp_sum <- colSums(y^2)
    m$uj <- (1 - eps^2 ) * tmp_sum / nrow(data) + eps^2 * rowSums(weights^2)

    if(quick == TRUE & max(m$uj) >= 1 ){
        return(FALSE)    #may need to change this later
    }

    tmp_dot <- t(data) %*% y # differences from python here - perhaps round to 32bit precision ?

    m$rho <- (1 - eps^2 ) * t(tmp_dot) / nrow(data) + eps^2 * weights # m by nv
    m$ry <- weights %*% t(m$rho) # normalized covariance of Y
    m$'Y_j^2' <- yscale^2 / (1 - m$uj)
    diag(m$ry) <- 1
    m$invrho <- 1 / (1 - m$rho^2)
    m$rhoinvrho <- m$rho * m$invrho
    m$Qij <- m$ry %*% m$rhoinvrho
    m$Si <- colSums(m$rho * m$rhoinvrho)

    #tmp <- (m$Qij - t(m$Si * t(m$rho)))
    tmp <- (m$Qij - m$Si * m$rho)

    m$'Qi-Si^2' <- colSums(m$rhoinvrho * tmp )


    # This is the objective, a lower bound for TC
    m$TC <- sum(log(1 + m$Si)) - 0.5 * sum(log(1 + m$`Qi-Si^2`)) +
        0.5 * sum(log(1 - m$uj))

    if(quick == FALSE){
        m$MI <- -0.5 * log(1 - m$rho^2)
        m$'X_iY_j' <- t(m$rho) * sqrt(m$`Y_j^2`)

        # For m$'X_iZ_j' using Julia because R solve will crash sometimes
        m$'X_iZ_j' <- t(solve(m$ry, m$rho))
        #s <- chol(m$ry)
        #m$'X_iZ_j' <- t(backsolve(s, forwardsolve(t(s), m$rho)))
        #julia_assign("ry", structure(m$ry, class = "JuliaFloat32"))
        #julia_assign("rho", structure(m$rho, class = "JuliaFloat32"))
        #m$'X_iZ_j' <- t( julia_eval("ry \\ rho", "R") )

        #einsum calculation
        es_res <- rep(0, dim(m$'X_iZ_j')[1])
        for(i in 1:dim(m$'X_iZ_j')[1]){
            for(j in 1:dim(m$'X_iZ_j')[2]){
                es_res[i] <- es_res[i] + m$X_iZ_j[i,j] * m$rho[j,i]
            }
        }
        m$'X_i^2 | Y' <- 1 - es_res
        m$'X_i^2 | Y'[ m$'X_i^2 | Y' < 1e-6 ] <- 1e-6
        m$'I(Y_j ; X)' <- 0.5 * log(m$`Y_j^2`) - 0.5 * log(yscale^2)
        m$'I(X_i ; Y)' <- -0.5 * log(m$`X_i^2 | Y`)
        m$TCs <- rowSums(m$MI) - m$`I(Y_j ; X)`
        # Next, a direct calculation of TC where each variable is in exactly one group.
        m$TC_no_overlap <- sum(matrixStats::colMaxs(m$MI)) - sum(m$`I(Y_j ; X)`)
        # Next, a direct calculation of TC. Should be upper bound for "TC", "TC_no_overlap"
        m$TC_direct <- sum(m$`I(X_i ; Y)`) - m$`I(Y_j ; X)`
        m$additivity <- sum(colSums(m$MI) - m$`I(X_i ; Y)`)
    }

    return(m)
}




