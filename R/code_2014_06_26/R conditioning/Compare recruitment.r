##########################################################################
#Compares the recruitment and recruitment deviates
#Programmed by Trevor A. Branch 20 July 2009
##########################################################################
compare.recruitment <- function(compare.objects,lab.cex=1,R.ylim=NULL,Rdev.ylim=NULL,label,legend=NULL) {
   par(mfrow=c(2,1),oma=c(3,3,3,1),mar=c(1,1,0,1))
   require(gplots)
   nobjects <- length(compare.objects) 

   plot.col <- rich.colors(n=nobjects)
   
   for (i in 1:nobjects) {
      x <- compare.objects[[i]]
      years <- (x$years[1]):(x$years[2]+1)
      rec <- x$Recruitment
      if(is.null(R.ylim)) {
        R.ylim <- c(0,1.1*max(rec))/1e06
      }
      if (i != 1) {
         par(new=T)
      }
      plot(years, rec/1e06, type="l",lwd=1,col=plot.col[i], ylim=R.ylim, las=1,yaxs="i",xlab="",ylab="",axes=F)
      par(new=T)
      plot(years, rec/1e06, type="p",cex=1,pch=19,col=plot.col[i], ylim=R.ylim, las=1,yaxs="i",xlab="",ylab="",axes=F)
   }
   mtext(outer=F, side=2,line=3,"Recruitment (millions)",cex=1)
   if (!is.null(legend)) {
      legend(x=1980,y=0.8*R.ylim[2],legend=legend,lty=1,lwd=2,col=plot.col,bty="n")
   }
   axis(2,las=1)
   box()   
   
   for (i in 1:nobjects) {
      x <- compare.objects[[i]]
      years <- (x$years[1]):(x$years[2])
      recdev <- x$tau
      if(is.null(Rdev.ylim)) {
        Rdev.ylim <- 1.1*c(-1*max(abs(recdev)),max(abs(recdev)))
      }
      if (i != 1) {
         par(new=T)
         axes <- F
      }
      else {
         axes <- T
      }
      plot(years, recdev, type="l",lwd=1,col=plot.col[i], ylim=Rdev.ylim, las=1,yaxs="i",xlab="",ylab="",axes=axes)
      par(new=T)
      plot(years, recdev, type="p",cex=1,pch=19,col=plot.col[i], ylim=Rdev.ylim, las=1,yaxs="i",xlab="",ylab="",axes=F)
   }
   mtext(outer=F, side=2,line=3,"Recruitment deviates",cex=1)
   mtext(side=1,line=3,"Year",cex=1.2, outer=T)
   mtext(side=3,line=1,label,cex=1.2, outer=T)
}
#pdf(file="figs\\Fig 17 Compare recruitment.pdf", width=6, height=6)
#compare.recruitment(compare.objects=data.base5hsqrt[1:8],label="example",legend=names(data.base5hsqrt[1:8]))
#dev.off()
