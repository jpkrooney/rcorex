#' @title check_converged
#' @description Internal function to check CorEx convergence. For the first 10 iterations convergence is not checked. After this, the absolute mean difference between the 5 most recent estimations of TC, and the 5 iterations previous to that again is calculated. Convergence is considered achieved if the difference is less than the value of the eps variable.
#' @param tc_history A list recording the calculated total correlation for each loop iteration.
#' @param eps Defines the change in total correlation (TC) needed to signal convergence.
#' @return Returns a boolean with TRUE signalling convergence and FALSE signalling no convergence.
#' @keywords internal
#'
check_converged <- function(tc_history, eps){
    len <- length(tc_history)
    if (len < 10){
        return(FALSE)
    } else {
        dist = -mean(unlist(tc_history[(len-10):(len-5)])) + mean(unlist(tc_history[(len-5):len]))
        return(abs(dist) < eps)
    }
}
