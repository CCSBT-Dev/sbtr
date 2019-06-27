# use Butler-Stephens algorithm to get probability
# of a sum of non-equal probability binomial variables

sumofbinomPOP <- function(df,s,phi) {

  # do the "X1+X2" bit first
  n1 <- df$nC[1]
  n2 <- df$nC[2]
  cyr <- df$y[1]
  ca <- df$a[1]
  c <- df$c[1]
  ba <- max(0,ca-(cyr-c))
  if(ba>30) ba <- 30
  pp1 <- (2*phi[as.character(ba),as.character(c)])/s[as.character(c)]
  p1 <- dbinom(0:n1,n1,pp1)
  cyr <- df$y[2]
  ca <- df$a[2]
  c <- df$c[2]
  ba <- max(0,ca-(cyr-c))
  if(ba>30) ba <- 30
  pp2 <- (2*phi[as.character(ba),as.character(c)])/s[as.character(c)]
  p2 <- dbinom(0:n2,n2,pp2) 
  # to stop stack overflow whittle it down to non-zero stuff
  p1 <- p1[p1>0]
  p2 <- p2[p2>0]
  psum <- comb.distro(p1,p2)
  # and whittle that down too!
  psum <- psum[psum>0]
  # now follow up the whole data frame
  xstop <- dim(df)[1]
  for(k in 3:xstop) {
    ntmp <- df$nC[k]
    cyr <- df$y[k]
    ca <- df$a[k]
    c <- df$c[k]
    ba <- max(0,ca-(cyr-c))
    if(ba>30) ba <- 30
    pptmp <- (2*phi[as.character(ba),as.character(c)])/s[as.character(c)]
    ptmp <- dbinom(0:ntmp,ntmp,pptmp)
    ptmp <- ptmp[ptmp>0]
    psum <- comb.distro(ptmp,psum)
    psum <- psum[psum>0]
  }

  return(psum)
}

# code to combine two distros

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
