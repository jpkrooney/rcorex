#' Missing values in the data, x, are indicated by v. Wherever this value appears in x, it is replaced by the mean value taken from the marginal distribution of that column
#'
#'#'@keywords internal
lc_mean_impute <- function(data){
    nsamp <- nrow(data)
    nv <- ncol(data)

    n_obs <- colSums(!is.na(data))

    # replace missing values with column means
    for(i in 1:ncol(data)){
        data[is.na(data[,i]), i] <- mean(data[,i], na.rm = TRUE)
    }
    return(list(data = data,
                n_obs = n_obs))
}



#' Missing values in the data, x, are indicated by v. Wherever this value appears in x, it is replaced by a random value taken from the marginal distribution of that column
#'@keywords internal

lc_random_impute <- function(data){
    nsamp <- nrow(data)
    nv <- ncol(data)

    n_obs <- colSums(!is.na(data))

    # replace missing values with random choice from that column
    for(i in 1:ncol(data)){
        data[is.na(data[,i]), i] <- sample(data[,i][!is.na(data[,i])],
                                           sum(is.na(data[,i])) )
    }
    return(list(data = data,
                n_obs = n_obs))
}

