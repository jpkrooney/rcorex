#' @title estimate_sig
#' @description Internal Function to calculate bootstrap estimate of mean and standard error of gaussian parameters
#' @param x_select Selected data
#' @param p_y_given_x_3d A 3D array of numerics in range (0, 1), that represent the probability that each observed x variable belongs to n_hidden latent variables of dimension dim_hidden. p_y_given_x_3d has dimensions (n_hidden, n_samples, dim_hidden).
#' @param term Intermediate term calculated by parent function
#' @return REturns bootstrapped estimates
#' @keywords internal
#'
estimate_sig <- function(x_select, p_y_given_x_3d, term){

    Gs <- list()
    for(i in 1:20){
        order <- sample(1: dim(p_y_given_x_3d)[2] )
        #counts <- tensor(x_select, p_y_given_x_3d[, order, ], alongA = 2, alongB=2)
        perm_p <- aperm(p_y_given_x_3d[, order, ], c(2, 1, 3))
        dims_p <- dim(perm_p)
        dim(perm_p) <- c(dims_p[1], prod(dims_p[2:3]))
        counts <- x_select %*% perm_p
        dim(counts) <- c(nrow(x_select), dims_p[2:3]) # c(dim_visible, nhid, dimhid)

        temp_counts2 <- counts * (log(counts) - term)
        Gs[[i]] <- 2* apply( ifelse( counts >0, temp_counts2, 0 ), c(2,3), sum)
    }
    Gs <- array(as.numeric(unlist(Gs)), dim=c(dim(Gs[[1]]), 20 ) )
    out <- apply(Gs, c(1,2), mean)
    return(out)
}

