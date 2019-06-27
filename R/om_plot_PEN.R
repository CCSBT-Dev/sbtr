#' Plot the likelihoods by steepness values, using Paige's code and idea.
#'
#' Plots the penalties for different penalty components and the total
#' split out by different values of steepness. 
#' Idea and original code: Paige Eveson
#' Code modified and rewritten by Trevor A Branch 6 September 2009  tbranch@gmail.com
#' Code blatantly copied and minimally adapted by Rich Hillary Jul 2012
#' read in all the files into an object (takes time)
#' source("get.all.data.r")
#' data.M4constrained <- get.all.files(directory="M4constrained\\arc\\c1s1l1sqrt")
#' get table of likelihood components and steepness values (quick)
#' source("TableLikelihoodComponents.r")
#' LikeM4const  <- likelihood.table(data.objects = data.M4constrained)
#' LikeM4unconst <- likelihood.table(data.objects = data.M4unconst)
#' LikeMpower <- likelihood.table(data.objects = data.Mpower)
#'
#' @export
#'
plot_pen_by_steepness <- function(pentable, caption) {
   par(mfrow=c(2,3),mai=c(0.1,0.2,0.2,0.2),omi=c(0.6,0.6,0.4,0))
   steep.vals <- sort(unique(pentable[,"steepness"]))

   x <- as.vector(pentable[,"M10"])
   x.lev <- sort(unique(x))
   ncolors <- length(x.lev)
   col.vec <- vector(length=length(x.lev))
   for (i in 1:length(x.lev)) {
      col.vec[x==x.lev[i]] <- i   
   }
   #print(col.vec)
   
   plot.nums <- c(1:5)
   nplots <- length(plot.nums)
   for(iii in 1:nplots) {  #the penalty components
      i <- plot.nums[iii]
      diffy <- max(pentable[,i]) - min(pentable[,i])
      
      if (diffy < 2.2) {
         midd <- mean(c(min(pentable[,i]),max(pentable[,i])))
         ylim <- c(midd-1.1,midd+1.1)
      }
      else {
         ylim <- c(min(pentable[,i]),max(pentable[,i])+0.1*diffy)
      }
      plot(x=jitter(pentable[,"steepness"],amount=0.015),y=pentable[,i],xlab="",ylab="",las=1,axes=F,ylim=ylim,pch=19,cex=1,col=col.vec+1)
      box()
      axis(2,las=1)
      if (iii>5) {
         axis(1,at=steep.vals)
      }
      coords <- par("usr")
      abline(h=mean(coords[3:4])-1,lty=2)
      abline(h=mean(coords[3:4])+1,lty=2)
      #par(xpd=T)
      #arrows(x0=coords[1],x1=coords[1],y0=coords[3],y1=min(coords[4],coords[3]+1),col="green",lwd=2,length=0)
      #par(xpd=F)
      mtext(outer=F,line=-2, colnames(pentable)[i],cex=1.2)
      if(iii==1) {  #do legend
         legend(x=coords[1],y=coords[4],bty="n",pch=19,col=(1:ncolors)+1,legend=paste("M10 =",x.lev))
      }
   }
   mtext(side=1,"Steepness",outer=T,line=3,cex=1.3)
   mtext(side=2,"Penalty",outer=T,line=2.5,cex=1.3)
   mtext(side=3,caption,outer=T,line=0.5,cex=1.3)
}


#' now for M0
#'
#' @export
#'
plot_pen_by_M0 <- function(pentable,caption) {
   par(mfrow=c(2,3),mai=c(0.1,0.2,0.2,0.2),omi=c(0.6,0.6,0.4,0))
   M0.vals <- sort(unique(pentable[,"M1"]))

   x <- as.vector(pentable[,"steepness"])
   x.lev <- sort(unique(x))
   ncolors <- length(x.lev)
   col.vec <- vector(length=length(x.lev))
   for (i in 1:length(x.lev)) {
      col.vec[x==x.lev[i]] <- i   
   }
   #print(col.vec)
   
   plot.nums <- c(1:5)
   nplots <- length(plot.nums)
   for(iii in 1:nplots) {  #the penalty components
      i <- plot.nums[iii]
      diffy <- max(pentable[,i]) - min(pentable[,i])
      
      if (diffy < 2.2) {
         midd <- mean(c(min(pentable[,i]),max(pentable[,i])))
         ylim <- c(midd-1.1,midd+1.1)
      }
      else {
         ylim <- c(min(pentable[,i]),max(pentable[,i])+0.1*diffy)
      }
      plot(x=jitter(pentable[,"M1"],amount=0.015),y=pentable[,i],xlab="",ylab="",las=1,axes=F,ylim=ylim,pch=19,cex=1,col=col.vec+1)
      box()
      axis(2,las=1)
      if (iii>5) {
         axis(1,at=M0.vals)
      }
      coords <- par("usr")
      abline(h=mean(coords[3:4])-1,lty=2)
      abline(h=mean(coords[3:4])+1,lty=2)
      #par(xpd=T)
      #arrows(x0=coords[1],x1=coords[1],y0=coords[3],y1=min(coords[4],coords[3]+1),col="green",lwd=2,length=0)
      #par(xpd=F)
      mtext(outer=F,line=-2, colnames(pentable)[i],cex=1.2)
      if(iii==1) {  #do legend
         legend(x=coords[1],y=coords[4],bty="n",pch=19,col=(1:ncolors)+1,legend=paste("h =",x.lev))
      }
   }
   mtext(side=1,expression(M[0]),outer=T,line=3,cex=1.3)
   mtext(side=2,"Penalty",outer=T,line=2.5,cex=1.3)
   mtext(side=3,caption,outer=T,line=0.5,cex=1.3)
}


#' now for M10
#'
#' @export
#'
plot_pen_by_M10 <- function(pentable,caption) {
   par(mfrow=c(2,3),mai=c(0.1,0.2,0.2,0.2),omi=c(0.6,0.6,0.4,0))
   M10.vals <- sort(unique(pentable[,"M10"]))

   x <- as.vector(pentable[,"steepness"])
   x.lev <- sort(unique(x))
   ncolors <- length(x.lev)
   col.vec <- vector(length=length(x.lev))
   for (i in 1:length(x.lev)) {
      col.vec[x==x.lev[i]] <- i   
   }
   #print(col.vec)
   
   plot.nums <- c(1:5)
   nplots <- length(plot.nums)
   for(iii in 1:nplots) {  #the penalty components
      i <- plot.nums[iii]
      diffy <- max(pentable[,i]) - min(pentable[,i])
      
      if (diffy < 2.2) {
         midd <- mean(c(min(pentable[,i]),max(pentable[,i])))
         ylim <- c(midd-1.1,midd+1.1)
      }
      else {
         ylim <- c(min(pentable[,i]),max(pentable[,i])+0.1*diffy)
      }
      plot(x=jitter(pentable[,"M10"],amount=0.015),y=pentable[,i],xlab="",ylab="",las=1,axes=F,ylim=ylim,pch=19,cex=1,col=col.vec+1)
      box()
      axis(2,las=1)
      if (iii>5) {
         axis(1,at=M10.vals)
      }
      coords <- par("usr")
      abline(h=mean(coords[3:4])-1,lty=2)
      abline(h=mean(coords[3:4])+1,lty=2)
      #par(xpd=T)
      #arrows(x0=coords[1],x1=coords[1],y0=coords[3],y1=min(coords[4],coords[3]+1),col="green",lwd=2,length=0)
      #par(xpd=F)
      mtext(outer=F,line=-2, colnames(pentable)[i],cex=1.2)
      if(iii==1) {  #do legend
         legend(x=coords[1],y=coords[4],bty="n",pch=19,col=(1:ncolors)+1,legend=paste("h =",x.lev))
      }
   }
   mtext(side=1,expression(M[10]),outer=T,line=3,cex=1.3)
   mtext(side=2,"Penalty",outer=T,line=2.5,cex=1.3)
   mtext(side=3,caption,outer=T,line=0.5,cex=1.3)
}
