#######################################################
#Plots the likelihoods for different likelihood components and the total
#split out by different values of steepness. 
#Idea and original code: Paige Eveson
#Code modified and rewritten by Trevor A Branch 6 September 2009  tbranch@gmail.com
#Modified by Trevor A. Branch 24 July 2013 to add two-decadal
#NLL components for recruitment deviates
#######################################################

#Plot the likelihoods by steepness values 
#using Paige's code and idea. 
plot.NLL.recdec <- function(NLLtable,caption) {
   par(mfrow=c(4,4),mai=c(0.1,0.2,0.2,0.2),omi=c(0.6,0.6,0.4,0))
   steep.vals <- sort(unique(NLLtable[,"steepness"]))
   
   x <- as.vector(NLLtable[,"M10"])
   x.lev <- sort(unique(x))
   ncolors <- length(x.lev)
   col.vec <- vector(length=length(x.lev))
   for (i in 1:length(x.lev)) {
      col.vec[x==x.lev[i]] <- i   
   }
   #print(col.vec)
   
   plot.nums <- c(1:9,11:13,14:17)
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
      plot(x=jitter(NLLtable[,"steepness"],amount=0.015),y=NLLtable[,i],xlab="",ylab="",las=1,axes=F,ylim=ylim,pch=19,cex=1,col=col.vec+1)
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
