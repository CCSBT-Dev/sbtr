#' @title Compares the CPUE by length for the model fits.
#' @export
#' 
CPUE.by.length <- function(data.objects, pt.scaling)
{
   nobj <- length(data.objects)

   for (k in 1:nobj) {
      cpue.pred <- data.objects[[k]]$cpue.pred
      yrs <- data.objects[[k]]$yrs.cpue
      
      length.list <- seq(from=data.objects[[k]]$lengths[1],
                         by=data.objects[[k]]$lengths[2],
                         length.out=data.objects[[k]]$lengths[3])
      
      len <- data.objects[[k]]$len.pred
      #extracts rows for years with CPUE data, and for LL1 (fishery = 1)
      LL1lens <- len[len[,2] <= yrs[2] & len[,2] >= yrs[1] & len[,1]==1,]
      LL1.prop <- LL1lens[,-c(1,2)]/rowSums(LL1lens[,-c(1,2)])  #just for rounding error, since some not exact
      rowSums(LL1.prop)  #test to make sure they add up to 1
      
      #this is model-predicted cpue by length category for LL1
      pred.cpue.by.length <- LL1.prop*cpue.pred
      
      #test to see if it worked
      #rowSums(pred.cpue.by.length)
      
      #now observed cpue by length category for LL1
      cpue.obs <- data.objects[[k]]$cpue
      len.obs <- data.objects[[k]]$len.obs
      LL1len.obs <- len.obs[len.obs[,2] <= yrs[2] & 
                               len.obs[,2] >= yrs[1] & len.obs[,1]==1,]
      LL1.prop.obs <- LL1len.obs[,-c(1,2)]/rowSums(LL1len.obs[,-c(1,2)])  #just for rounding error, since some not exact
      
      #this is observed cpue by length category for LL1
      obs.cpue.by.length <- LL1.prop.obs*cpue.obs
      
      #difference between observed and predicted cpue by length
      resids <- obs.cpue.by.length-pred.cpue.by.length
      
      nr <- nrow(resids)
      nc <- ncol(resids)
      
      plot(x=0,y=0, xlim=c(yrs[1], yrs[2]), 
           ylim=c(length.list[1],length.list[nc]), 
           xlab="Years", ylab="Lengths",cex.lab=1.8, cex.axis=1.3)
      yr.list <- seq(yrs[1], yrs[2], 1)
      length(resids[1,])
      
      for (i in 1:nr) {
         posneg <- sign(resids[i,])
         colors <- posneg
         colors[posneg>0] <- "#0000FF77"
         colors[posneg<0] <- "#FF000077"
         points(x=rep(yr.list[i],nc), y=length.list, type="p", 
                cex=sqrt(abs(resids[i,]*pt.scaling)),
                bg=colors, pch=21, col=NA)  
      }
      abline(v=1997.5,lty=2,col="gray50",lwd=2)
      mtext(side=3,line=1,outer=F, names(data.objects)[k], cex=1.3)
      print(k)
      print("Mean residuals by year")
      print(names(data.objects)[k])
      print(cbind(yr.list,rowMeans(resids)))
   }
}

