#' @title choose_maximal_run
#' @description Internal function to select the optimal corex run when biocroex is run with repeats > 1
#' @param repeat_results List of repeated corex results
#' @param repeats The number of repeat runs
#' @param max_iter Numeric that specifies the maximum number of iterations the corex fits were run. Used in \emph{choose_maximal_run()} to detect non-converged runs.
#' @return Returns the run with the highest TC of all converged runs. If no runs have converged, \emph{choose_maximal_run()} returns the run with maximal TC anyway - in such cases the results should not be interpreted, but may be useful for diagnostic purposes.
#' @keywords internal
#'
choose_maximal_run <- function(repeat_results, repeats, max_iter){
    # Examine convergence of biocorex runs and select best run
    if ( repeats ==1 ){
        out <- repeat_results[[1]]
    } else {
        states <- unlist(lapply(repeat_results, '[[', "state"))
        converged_repeats <- which( states == "Converged" )
        message( paste0( length(converged_repeats), " out of ", repeats,
                       " repeat runs of biocorex converged."))
        if( length(converged_repeats > 0 ))
        {
            message("Returning biocorex with highest TC of all converged runs - unconverged runs will not be included in comparison of runs.")
            repeat_tcs <- unlist( lapply( lapply(repeat_results, '[[', "tcs"), sum) )
            wm <- which.max( repeat_tcs[ converged_repeats ] )
            out <- repeat_results[[ converged_repeats[wm] ]]
        } else {
            warning("As no biocorex runs have converged, recommend to run again with increased max_iter, or use larger eps.\n Returning the unconverged biocorex with the highest TC for diagnostic purposes.\n It is not recommended to trust these results due to non-convergence.")
            repeat_tcs <- unlist( lapply( lapply(repeat_results, '[[', "tcs"), sum) )
            wm <- which.max( repeat_tcs )
            out <- repeat_results[[ wm ]]
        }
    }
    return(out)
}
