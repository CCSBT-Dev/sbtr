#########################################################
#Table of the likelihood components for a specified set of _lab.rep
#files.
#Trevor A. Branch 14 July 2009, tbranch@uw.edu
#Modified 24 July 2013 to to recruitment penalties by two-decades
# Modified by Ana to get AC from lab.rep and use it to compute SR penalties by decades
##################################################################
likelihood.recdec <- function(data.objects) {
   nobjects <- length(data.objects)
   result <- NULL
   objF <- NULL
   sumNLL <- NULL
   steep <- NULL
   M1<- NULL
   M10 <- NULL
   recdec <- NULL
   AC <- 0    # need to change this to actual value if using old lab.rep that do not have AC 
   xx <- data.objects[[1]]
   if(!is.null(xx$AC_penalty)) AC <- xx$AC_penalty
   for (i in 1:nobjects)
   {
      xx <- data.objects[[i]]
      
      sumNLL <- rbind(sumNLL,sum(xx$lnlike))
      objF <- rbind(objF,xx$ObjF)
      steep <- rbind(steep,xx$steep)
      result <- rbind(result, xx$lnlike)
      M1<- rbind(M1,xx$M[2])
      M10 <- rbind(M10,xx$M[11])
      
      var_esp <- (1-AC^2)*0.6^2
      recdec <- rbind(recdec, c(sum(xx$Reps[1:19]^2)/(2*var_eps),
                                sum(xx$Reps[20:39]^2)/(2*var_eps),
                                sum(xx$Reps[40:59]^2)/(2*var_eps),
                                sum(xx$Reps[60:79]^2)/(2*var_eps)))
      
      rownames(result)[i] <- names(data.objects)[i]
   }
   AC <- rep(AC,nobsjects)
   ncols <- ncol(result)
   result<- cbind(result,sumNLL,objF,recdec,steep,M1,M10,AC)
   nresults <- ncol(result)
   #print(nresults)
   #print(colnames(result))
   colnames(result)[(nresults-9+1):nresults] <- 
         c("sumNLL","objF",
           "rec30s-40s" ,"rec50s-60s" ,"rec70s-80s","rec90s-00s",
           "steepness","M1","M10","AC")
   return(result)
}
