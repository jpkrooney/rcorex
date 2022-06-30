#


lc_update_records <- function(histlc, moments, delta, verbose){
    # Print and store some statistics about each iteration
    histlc$TC <- c(histlc$TC, moments$TC)
    histlc$TCs <- c(histlc$TCs, moments$TCs)
    histlc$additivity <- c(histlc$additivity, moments$additivity)

    if(verbose == TRUE){
        print(paste0("TC: ", format(moments$TC, digits=4), ". Additivity: ",
                     format(moments$additivity, digits = 4), ". Delta: ",
                     format(delta, digits=4)))
    }
    return(histlc)
}

