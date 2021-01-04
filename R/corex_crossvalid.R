

###### Possible label switching problem needs to be addressed!


corex_crossvalid <- function(corex_obj, k){
    data <- corex_obj$data

    idx <- sample(1:k, nrow(data), replace = TRUE)

    # Make k training and test datasets
    datasets <- lapply(1:k, function(i) data[ idx!=i, ])
    testsets <- lapply(1:k, function(i) data[idx==i, ])

    # Apply corex to each training set
    trained_corexes <- lapply(1:k, function(i) {
        tempcall <- corex_obj$call
        tempcall$data <- datasets[[i]]
        message(paste0("Fitting fold: ", i))
        suppressMessages( eval(tempcall) )
    })

    # Use trained corexes to transform each test dataset
    test_results <- lapply(1:k, function(i)
        transform_corex(trained_corexes[[i]], new_data = testsets[[i]] ) )

    # Compile test results into original data order
    test_labels <- matrix( nrow = nrow(corex_obj$labels),
                           ncol = ncol(corex_obj$labels))
    for(i in 1:k) {
        test_labels[idx == i ,] <- test_results[[i]]
    }

    # Compare test_labels to corex_obj$labels
    res <- table(corex_obj$labels == test_labels)
    res <- res[2] / (res[1] + res[2])
    names(res) <- NULL
    return(res)
}
