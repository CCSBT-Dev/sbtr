#####################################################
#Compare the B10+, SSB, and R for multiple individual cells
#basically think of comparing one mid cell from multiple
#different runs for a particular sensitivity
#abs.rel: if "rel" then all plotted rel to unfished, e.g. SSB0, R0, B10+(0)
#         if "abs" then absolute rec etc are plotted
#####################################################

plot.B10.SSB.R.compare <- function(objects, label="", legend=NULL,
                                   abs.rel=c("rel","abs")) {
   abs.rel<-abs.rel[1]
   
   par(mfrow=c(3,1),oma=c(5,0,3,1),mar=c(0,5,1,0))
   require(gplots)
   nobjects <- length(objects) 
   colors <- rich.colors(nobjects+2)[-c(1,nobjects+2)]
   
   nssb <- length(objects[[1]]$Sbio)
   years <- c(objects[[1]]$years[1]:(objects[[1]]$years[2]+1))
   depl <- matrix(nrow=nobjects, ncol=nssb)
   for (i in 1:nobjects) {
      x <- objects[[i]]
      if (abs.rel=="rel") {
         depl[i,] <- x$Sbio/x$Sbio[1]   #since first element is SSB0
         ylab <- "SSB/unfished"
      } else {
         depl[i,] <- x$Sbio/1e06
         ylab <- "SSB(million)"
      }
   }
   plot(x=years,y=depl[1,],type="l", ylim=c(0,1.1*max(depl)), col=colors[1],
        lwd=2,xaxt="n",lty=1, yaxs="i", xaxs="i", las=1, ylab="", xlab="")
   mtext(side=2,outer=F,line=2.8,ylab)
   for (i in 2:nobjects) {
      lines(x=years,y=depl[i,], col=colors[i], lwd=2,lty=1)
   }
   mtext(side=3,line=1,label,cex=1.2, outer=F)

   #==========legend drawing
   if (!is.null(legend)) {
      legend(x="topright", legend=legend, col=colors, lwd=2, 
             bty="n", cex=1.3)   
   }
   
   #========B10+
   nssb <- length(objects[[1]]$B10_plus)
   years <- c(objects[[1]]$years[1]:(objects[[1]]$years[2]+1))
   depl <- matrix(nrow=nobjects, ncol=nssb)
   for (i in 1:nobjects) {
      x <- objects[[i]]
      if (abs.rel=="rel") {
         depl[i,] <- x$B10_plus/x$B10_plus[1]   #since first element is SSB0
         ylab <- "B10+/unfished"
      }
      else {
         depl[i,] <- x$B10_plus/1e06
         ylab <- "B10+ (million)"
      }
   }
   plot(x=years,y=depl[1,],type="l", ylim=c(0,1.1*max(depl)), col=colors[1],
        lwd=2,xaxt="n",lty=1, yaxs="i", xaxs="i", las=1, ylab="", xlab="")
   for (i in 2:nobjects) {
      lines(x=years,y=depl[i,], col=colors[i], lwd=2,lty=1)
   }
   mtext(side=2,outer=F,line=2.8,ylab)
   
   #========Recruitment
   nssb <- length(objects[[1]]$Recruitment)
   years <- c(objects[[1]]$years[1]:(objects[[1]]$years[2]+1))
   depl <- matrix(nrow=nobjects, ncol=nssb)
   for (i in 1:nobjects) {
      x <- objects[[i]]
      if (abs.rel=="rel") {
         depl[i,] <- x$Recruitment/x$Recruitment[1]   #since first element is SSB0
         ylab <- "Rec/unfished"
      }
      else {
         depl[i,] <- x$Recruitment/1e06
         ylab <- "Rec (million)"
      }
   }
   plot(x=years,y=depl[1,],type="l", ylim=c(0,1.1*max(depl)), col=colors[1],
        lwd=2,lty=1, yaxs="i", xaxs="i", las=1, ylab="", xlab="")
   for (i in 2:nobjects) {
      lines(x=years,y=depl[i,], col=colors[i], lwd=2,lty=1)
   }
   mtext(side=2,outer=F,line=2.8,ylab)
   
   mtext(side=1,outer=T,"Year",cex=1.2, line=3)
}
#source("get.all.files.r")
#data.base.NS <- get.all.files("arc\\base2010sqrtns")
