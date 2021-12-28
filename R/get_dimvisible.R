#' @title Get dim_visible from supplied data
#' @description Internal function to determine the dimension of supplied discrete data
#' @details
#' This function determines the dimension of unique values in the supplied data.
#' @param data Data provided by user. For biocorex data can either be continuous (gaussian) or discrete (consectutive integers 0, 1, 2, 3...etc). Data types cannot by mixed in this implementation.
#' @param max_dim Sets the maximum dimension allowed for discrete data. Default = 31
#' @return An integer that represents the dimension of unique values in the discrete data.
#'
#' @keywords internal
#'

get_dimvisible <- function(data, max_dim = 31){
    values_in_data <- unique(sort(unlist(data))) # Get the set of unique values in the data
    values_in_data <- values_in_data[!is.na(values_in_data)] # remove NA if it is there
    dim_visible <- max(values_in_data) + 1
    if( ! all( seq(0, (dim_visible - 1)) == values_in_data) ) {
        stop("Data matrix values should be consecutive integers starting with 0,1,...")
    }
    if ( max(values_in_data >max_dim )){
        stop("Discrete valued variables can only take values from 0 to ", max_dim)
    }
    return(dim_visible)
}
