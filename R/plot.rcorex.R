#' @title plot.rcorex
#' @description Function to plot corex TC per hidden factor vs iterations. Visualises algorithm convergence.
#' @param x A fit corex model object
#' @param ... Not used
#' @return A ggplot2 graph object
#' @import ggplot2
#' @export
#'
plot.rcorex <- function(x, ...) {

    history <- data.frame(matrix(unlist(x$tc_history), nrow=length(x$tc_history), byrow=T))
    iters <- x$iterations
    n_hidden <- dim(history)[2]
    labels <- paste0("X", 1:n_hidden)
    totals <- rowSums(history)
    cluster <- NULL ; tcs <- NULL # added to prevent compile time note: 'no visible binding for global variable ....'

    df1 <- data.frame(iters = 1:iters,
                      cluster = rep( labels, each = iters),
                      tcs = unlist(history))
    df1 <- rbind(df1, data.frame(iters = 1:iters,
                                 cluster = "Total",
                                 tcs = totals))

    g <- ggplot(data = subset(df1, cluster != "Total"), aes(x=iters, y = tcs, col=cluster)) +
        geom_line() +
        geom_line(data = subset(df1, cluster == "Total"), colour="black", size=2) +
        theme_minimal() + ggtitle("Corex fit convergence")
    return(g)
}
