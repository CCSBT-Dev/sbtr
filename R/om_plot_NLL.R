#' @title Plots the likelihoods for different likelihood components.
#' @description
#' Plot the likelihoods by steepness values, using Paige Evesons code and idea. Plots the likelihoods for different likelihood components and the total split out by different values of steepness. Code modified by Trevor A Branch 6 September 2009.
#' @export
#'
plot_NLL_by_steepness <- function(NLLtable, caption)
{
   par(mfrow=c(3,5), mai=c(0.1,0.2,0.2,0.2), omi=c(0.6,0.6,0.4,0))
   steep.vals <- sort(unique(NLLtable[,"steepness"]))
   amountji <- (max(steep.vals)-min(steep.vals))/length(steep.vals)/2

   x <- as.vector(NLLtable[,"M10"])
   x.lev <- sort(unique(x))
   ncolors <- length(x.lev)
   col.vec <- vector(length=length(x.lev))
   for (i in 1:length(x.lev))
   {
      col.vec[x==x.lev[i]] <- i   
   }
   #print(col.vec)
   
   plot.nums <- c(1:9, 11:14)
   nplots <- length(plot.nums)
   for(iii in 1:nplots) {  #the likelihood components
      if(iii==1) {  #do legend
         plot(x=1,y=1,type="n",axes=F,ann=F,ylim=c(0,2),xlim=c(0,2))
         legend(x=0,y=2.45,bty="n",horiz=T,
                pch=19,cex=1.3,col=(1:ncolors)+1,
                legend=paste("M10 =",x.lev),
                xpd=NA)
         par(new=T)
      }

      i <- plot.nums[iii]
      diffy <- max(NLLtable[,i]) - min(NLLtable[,i])
      
      if (diffy < 2.2) {
         midd <- mean(c(min(NLLtable[,i]),max(NLLtable[,i])))
         ylim <- c(midd-1.1,midd+1.1)
      }
      else {
         ylim <- c(min(NLLtable[,i]),max(NLLtable[,i])+0.1*diffy)
      }
      plot(x=jitter(NLLtable[,"steepness"],amount=amountji),y=NLLtable[,i],xlab="",ylab="",las=1,axes=F,ylim=ylim,pch=19,cex=1,col=col.vec+1)
      box()
      axis(2,las=1)
      if (iii>8) {
         axis(1,at=steep.vals)
      }
      coords <- par("usr")
      abline(h=mean(coords[3:4])-1,lty=2)
      abline(h=mean(coords[3:4])+1,lty=2)
      #par(xpd=T)
      #arrows(x0=coords[1],x1=coords[1],y0=coords[3],y1=min(coords[4],coords[3]+1),col="green",lwd=2,length=0)
      #par(xpd=F)
      mtext(outer=F,line=-2, colnames(NLLtable)[i],cex=1.2)
   }
   mtext(side=1,"Steepness",outer=T,line=3,cex=1.3)
   mtext(side=2,"Negative log likelihood",outer=T,line=2.5,cex=1.3)
   mtext(side=3,caption,outer=T,line=0.5,cex=1.3)
}


#' Predicted length frequency for longline fishery 1 to the observed data.
#'
#' Plots the likelihoods for different likelihood components and the
#' total split out by different values of M10. Idea and original code:
#' Paige Eveson. Code modified and rewritten by Trevor A Branch 6
#' September 2009. Modified 24 June 2014 Trevor A Branch To split out
#' by different values of M10 instead of steepness.  Plot the
#' likelihoods by M10 values, using Paige's code and idea.
#'
#' @export
#' 
plot_NLL_by_M <- function(NLLtable, caption)
{
   par(mfrow=c(3,5),mai=c(0.1,0.2,0.2,0.2),omi=c(0.6,0.6,0.4,0))
   M10.vals <- sort(unique(NLLtable[,"M10"]))
   amountji <- (max(M10.vals)-min(M10.vals))/length(M10.vals)/2
   
   x <- as.vector(NLLtable[,"steepness"])
   x.lev <- sort(unique(x))
   ncolors <- length(x.lev)
   col.vec <- vector(length=length(x.lev))
   for (i in 1:length(x.lev)) {
      col.vec[x==x.lev[i]] <- i   
   }
   #print(col.vec)
   
   plot.nums <- c(1:9,11:13)
   nplots <- length(plot.nums)
   for(iii in 1:nplots) {  #the likelihood components
      if(iii==1) {  #do legend
         plot(x=1,y=1,type="n",axes=F,ann=F,ylim=c(0,2),xlim=c(0,2))
         legend(x=0,y=2.45,bty="n",horiz=T,
                pch=19,cex=1.3,col=(1:ncolors)+1,
                legend=paste("h =",x.lev),
                xpd=NA)
         par(new=T)
      }
      
      i <- plot.nums[iii]
      diffy <- max(NLLtable[,i]) - min(NLLtable[,i])
      
      if (diffy < 2.2) {
         midd <- mean(c(min(NLLtable[,i]),max(NLLtable[,i])))
         ylim <- c(midd-1.1,midd+1.1)
      }
      else {
         ylim <- c(min(NLLtable[,i]),max(NLLtable[,i])+0.1*diffy)
      }
      plot(x=jitter(NLLtable[,"M10"],amount=amountji),y=NLLtable[,i],xlab="",
                     ylab="",las=1,axes=F,ylim=ylim,pch=19,cex=1,col=col.vec+1)
      box()
      axis(2,las=1)
      if (iii>8) {
         axis(1,at=M10.vals)
      }
      coords <- par("usr")
      abline(h=mean(coords[3:4])-1,lty=2)
      abline(h=mean(coords[3:4])+1,lty=2)
      #par(xpd=T)
      #arrows(x0=coords[1],x1=coords[1],y0=coords[3],y1=min(coords[4],coords[3]+1),col="green",lwd=2,length=0)
      #par(xpd=F)
      mtext(outer=F,line=-2, colnames(NLLtable)[i],cex=1.2)
   }
   mtext(side=1,"M10",outer=T,line=3,cex=1.3)
   mtext(side=2,"Negative log likelihood",outer=T,line=2.5,cex=1.3)
   mtext(side=3,caption,outer=T,line=0.5,cex=1.3)
}


#' Plot the likelihoods by M1 values, using Paige Evesons code and idea. Plots the likelihoods for different likelihood components and the total split out by different values of steepness.
#' @export
#'
plot_NLL_by_m <- function(NLLtable, caption)
{
   par(mfrow=c(3,5),mai=c(0.1,0.2,0.2,0.2),omi=c(0.6,0.6,0.4,0))
   M1.vals <- sort(unique(NLLtable[,"M1"]))
   amountji <- (max(M1.vals)-min(M1.vals))/length(M1.vals)/2

   x <- as.vector(NLLtable[,"steepness"])
   x.lev <- sort(unique(x))
   ncolors <- length(x.lev)
   col.vec <- vector(length=length(x.lev))
   for (i in 1:length(x.lev)) {
      col.vec[x==x.lev[i]] <- i   
   }
   #print(col.vec)
   
   plot.nums <- c(1:9,11:13)
   nplots <- length(plot.nums)
   for(iii in 1:nplots) {  #the likelihood components
      if(iii==1) {  #do legend
         plot(x=1,y=1,type="n",axes=F,ann=F,ylim=c(0,2),xlim=c(0,2))
         legend(x=0,y=2.45,bty="n",horiz=T,
                pch=19,cex=1.3,col=(1:ncolors)+1,
                legend=paste("h =",x.lev),
                xpd=NA)
         par(new=T)
      }
      
      i <- plot.nums[iii]
      diffy <- max(NLLtable[,i]) - min(NLLtable[,i])
      
      if (diffy < 2.2) {
         midd <- mean(c(min(NLLtable[,i]),max(NLLtable[,i])))
         ylim <- c(midd-1.1,midd+1.1)
      }
      else {
         ylim <- c(min(NLLtable[,i]),max(NLLtable[,i])+0.1*diffy)
      }
      plot(x=jitter(NLLtable[,"M1"],amount=amountji),y=NLLtable[,i],xlab="",
                     ylab="",las=1,axes=F,ylim=ylim,pch=19,cex=1,col=col.vec+1)
      box()
      axis(2,las=1)
      if (iii>8) {
         axis(1,at=M1.vals)
      }
      coords <- par("usr")
      abline(h=mean(coords[3:4])-1,lty=2)
      abline(h=mean(coords[3:4])+1,lty=2)
      #par(xpd=T)
      #arrows(x0=coords[1],x1=coords[1],y0=coords[3],y1=min(coords[4],coords[3]+1),col="green",lwd=2,length=0)
      #par(xpd=F)
      mtext(outer=F,line=-2, colnames(NLLtable)[i],cex=1.2)
   }
   mtext(side=1,"M1",outer=T,line=3,cex=1.3)
   mtext(side=2,"Negative log likelihood",outer=T,line=2.5,cex=1.3)
   mtext(side=3,caption,outer=T,line=0.5,cex=1.3)
}

#' Plot the likelihoods by M1 values, using Paige Evesons code and idea. Plots the likelihoods for different likelihood components and the total split out by different values of M10.
#' @export
#'
plot_NLL_by_mM <- function(NLLtable, caption)
{
   par(mfrow=c(3,5),mai=c(0.1,0.2,0.2,0.2),omi=c(0.6,0.6,0.4,0))
   M1.vals <- sort(unique(NLLtable[,"M1"]))
   amountji <- (max(M1.vals)-min(M1.vals))/length(M1.vals)/2

   x <- as.vector(NLLtable[,"M10"])
   x.lev <- sort(unique(x))
   ncolors <- length(x.lev)
   col.vec <- vector(length=length(x.lev))
   for (i in 1:length(x.lev)) {
      col.vec[x==x.lev[i]] <- i   
   }
   #print(col.vec)
   
   plot.nums <- c(1:9,11:13)
   nplots <- length(plot.nums)
   for(iii in 1:nplots) {  #the likelihood components
      if(iii==1) {  #do legend
         plot(x=1,y=1,type="n",axes=F,ann=F,ylim=c(0,2),xlim=c(0,2))
         legend(x=0,y=2.45,bty="n",horiz=T,
                pch=19,cex=1.3,col=(1:ncolors)+1,
                legend=paste("M10 =",x.lev),
                xpd=NA)
         par(new=T)
      }
      
      i <- plot.nums[iii]
      diffy <- max(NLLtable[,i]) - min(NLLtable[,i])
      
      if (diffy < 2.2) {
         midd <- mean(c(min(NLLtable[,i]),max(NLLtable[,i])))
         ylim <- c(midd-1.1,midd+1.1)
      }
      else {
         ylim <- c(min(NLLtable[,i]),max(NLLtable[,i])+0.1*diffy)
      }
      plot(x=jitter(NLLtable[,"M1"],amount=amountji),y=NLLtable[,i],xlab="",
                     ylab="",las=1,axes=F,ylim=ylim,pch=19,cex=1,col=col.vec+1)
      box()
      axis(2,las=1)
      if (iii>8) {
         axis(1,at=M1.vals)
      }
      coords <- par("usr")
      abline(h=mean(coords[3:4])-1,lty=2)
      abline(h=mean(coords[3:4])+1,lty=2)
      #par(xpd=T)
      #arrows(x0=coords[1],x1=coords[1],y0=coords[3],y1=min(coords[4],coords[3]+1),col="green",lwd=2,length=0)
      #par(xpd=F)
      mtext(outer=F,line=-2, colnames(NLLtable)[i],cex=1.2)
   }
   mtext(side=1,"M1",outer=T,line=3,cex=1.3)
   mtext(side=2,"Negative log likelihood",outer=T,line=2.5,cex=1.3)
   mtext(side=3,caption,outer=T,line=0.5,cex=1.3)
}


