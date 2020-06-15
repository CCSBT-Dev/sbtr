#' Table of the likelihood components for a specified set of _lab.rep files
#'
#' @export
#'
table_likelihood <- function(data.objects) {

   nobjects <- length(data.objects)
   likelihoods <- NULL
   penalties <- NULL
   objF <- NULL
   sumNLL <- NULL
   sumPEN <- NULL
   steep <- NULL
   M1 <- NULL
   M10 <- NULL

   for (i in 1:nobjects) {
      xx <- data.objects[[i]]
      sumNLL <- rbind(sumNLL, sum(xx$lnlike))
      sumPEN <- rbind(sumPEN, sum(xx$penal))
      objF <- rbind(objF, xx$ObjF)
      steep <- rbind(steep, xx$steep)
      likelihoods <- rbind(likelihoods, xx$lnlike)
      penalties <- rbind(penalties, xx$penal)
      M1 <- rbind(M1, xx$M[2])
      M10 <- rbind(M10, xx$M[11])
      rownames(likelihoods)[i] <- names(data.objects)[i]
   }

   ncols <- ncol(likelihoods)
   result <- cbind(likelihoods, sumNLL, penalties, sumPEN, objF, steep, M1, M10)
   nresults <- ncol(result)
   colnames(result) <- c(colnames(likelihoods), "sumNLL", colnames(penalties), "sumPEN", "objF", "steepness", "M1", "M10")
   return(result)
}
