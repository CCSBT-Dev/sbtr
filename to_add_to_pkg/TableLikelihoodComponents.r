#########################################################
#Table of the likelihood components for a specified set of _lab.rep files.
#Trevor A. Branch 14 July 2009
##################################################################
likelihood.table <- function(data.objects) {
   nobjects <- length(data.objects)
   result <- NULL
   objF <- NULL
   sumNLL <- NULL
   for (i in 1:nobjects)
   {
      xx <- data.objects[[i]]

      sumNLL <- rbind(sumNLL,sum(xx$lnlike))
      objF <- rbind(objF,xx$ObjF)
      result <- rbind(result, xx$lnlike)
      rownames(result)[i] <- names(data.objects)[i]
   }
   ncols <- ncol(result)
   result<- cbind(result,sumNLL,objF)
   return(result)
}

