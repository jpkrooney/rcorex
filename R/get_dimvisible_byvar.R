#' @title get_dimvisible_byvar from supplied data
#' @description Internal function to determine the dimension of supplied discrete by column
#' @details
#' This function determines the dimension of unique values in the supplied data for each column in the supplied data frame, if the marginal description = discrete for a given variable.
#' @param data Data provided by user. For biocorex data can either be continuous (gaussian) or discrete (consectutive integers 0, 1, 2, 3...etc). Data types cannot by mixed in this implementation.
#' @param marginal_description Character string which determines the marginal distribution of the data. Should be of length equal to the number of variables in data. get_dimvisible_byvar() calculates the dimension when the marginal_description = 'discrete'
#' @return An integer that represents the dimension of unique values in the discrete data.
#'
#' @keywords internal
#'

get_dimvisible_byvar <- function(data, marginal_description){

    dimvis_byvar <- rep(NA, length(marginal_description))

    if(length(marginal_description) == 1 ){
        stop("Function 'get_dimvisible_byvar()' requires marginal_descriptions for each variable.")
    } else {
        for(i in 1:length(marginal_description)){
            if(marginal_description[i] == "discrete"){
                dimvis_byvar[i] <- i
            }
        }
    }
}
