#' @title unique_info
#' @description Internal function
#' @param correct Index of correct predictions of hidden variable assignment obtained by cross-referencing max_indexes of p_y_given_x_3d and max_indices of log_marg_x_4d for each possible dim_hidden. It indicates whether the estimate based on x_i for y_j is correct for sample l.
#' @return Returns a vector of length n_hidden the represents an estimate of fraction of unique info in each n_hidden
#'
unique_info <- function(correct){
    n_hidden <- nrow(correct)
    n_samples <- ncol(correct)

    # sum correct over n_hidden
    total <- rowSums(correct)
    # apply minimum score to total of 1
    total[total < 1] <- 1
    # get the descending order
    ordered <- rev( order(total))

    unexplained <- rep(TRUE, n_samples)
    unique <- rep(0, n_hidden)

    for( j in ordered){
        unique[j] <- sum( unexplained * correct[j,] )
        unexplained <- unexplained & !correct[j, ]
    }

    frac_unique <- unique / total
    return(frac_unique)
}
