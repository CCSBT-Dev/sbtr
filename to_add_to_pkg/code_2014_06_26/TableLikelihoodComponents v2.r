#########################################################
#Table of the likelihood components for a specified set of _lab.rep files.
#Trevor A. Branch 14 July 2009
##################################################################
likelihood.table2 <- function(data.objects) {
   nobjects <- length(data.objects)
   result <- NULL
   objF <- NULL
   sumNLL <- NULL
   steep <- NULL
   M1<- NULL
   M10 <- NULL
   for (i in 1:nobjects)
   {
      xx <- data.objects[[i]]

      sumNLL <- rbind(sumNLL,sum(xx$lnlike))
      objF <- rbind(objF,xx$ObjF)
      steep <- rbind(steep,xx$steep)
      result <- rbind(result, xx$lnlike)
      M1<- rbind(M1,xx$M[2])
      M10 <- rbind(M10,xx$M[11])
      rownames(result)[i] <- names(data.objects)[i]
   }
   ncols <- ncol(result)
   result<- cbind(result,sumNLL,objF,steep,M1,M10)
   nresults <- ncol(result)
   #print(nresults)
   #print(colnames(result))
   colnames(result)[(nresults-5+1):nresults] <- c("sumNLL","objF","steepness","M1","M10")
   return(result)
}
