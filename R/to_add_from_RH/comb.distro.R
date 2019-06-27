comb.distro <- function(a, b) {

    # because of the following computation, make a matrix with more columns than rows
    if (length(a) < length(b)) {
        t <- a
        a <- b
        b <- t
    }

    # explicitly multiply the probability distributions
    m <- a %*% t(b)

    # initialized the final result, element 1 = count 0
    result <- rep(0, length(a)+length(b)-1)

    # add the probabilities, always adding to the next subsequent slice
    # of the result vector
    for (i in 1:nrow(m)) {
        result[i:(ncol(m)+i-1)] <- result[i:(ncol(m)+i-1)] + m[i,]
    }

    return(result)
}
