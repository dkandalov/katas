doors_puzzle <- function(ndoors=100,passes=100) {
    doors <- rep(FALSE,ndoors)
    for (ii in seq(1,passes)) {
        mask <- seq(0,ndoors,ii)
        doors[mask] <- !doors[mask]
    }
    return (which(doors == TRUE))
}
doors_puzzle2 <- function(ndoors=100,passes=100) {
    names(which(
        table(unlist(
            sapply(1:passes, function(X) seq(0, ndoors, by=X))
        )) %% 2 == 1
    ))
}

doors_puzzle()
