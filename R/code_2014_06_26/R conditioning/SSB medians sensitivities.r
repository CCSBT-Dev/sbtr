###################SSB medians#########################################
#Given multiple s4 files, plots median SSB
#Trevor A. Branch tbranch@gmail.com
#####################################################################
med.SSB.sensitivities <- function(s4files,B.ylim,xlim=c(1931,2031),leg.names=NULL,legend.x="topleft",points.on=F)  {
  require(gplots)
  nfiles <- length(s4files)
  
  plot.colors <- rich.colors(nfiles)
  symbs <- rep(c(1:2,5,15:18),10)
  
  for (i in 1:nfiles) {
    SSB.data <- read.table(s4files[i],skip=3)
    SSB.data <- as.matrix(SSB.data[,-c(1,2)])
    
    med.SSB <- apply(SSB.data[,1:101],MARGIN=2,quantile,0.5)/1000
    if (i >1) {
       par(new=T)
    }
    plot(x=1931:2031,med.SSB,type="l",lwd=2,col=plot.colors[i],ylim=B.ylim,xlim=xlim,yaxs="i",xaxs="i",
         xlab="",ylab="",axes=F)
    if (points.on==T) {
      par(new=T)
      plot(x=1931:2031,med.SSB,type="p",pch=symbs[i],col=plot.colors[i],ylim=B.ylim,xlim=xlim,yaxs="i",xaxs="i",
           xlab="",ylab="",axes=F)
    }
  }
  box()
  axis(1)
  axis(2,las=1)
  mtext(side=1,"Year",cex=1.3,line=2.5)
  mtext(side=2,"Spawning stock biomass (thousand mt)",cex=1.3,line=3.3)
  mtext(side=3,"Sensitivity tests 11810",cex=1.3,line=0.5)
  legend(x=legend.x,legend=leg.names,col=plot.colors,lwd=2,cex=1,bty="n")
  if (points.on==T) {
     legend(x=legend.x,legend=rep("",nfiles),col=plot.colors,pch=symbs[nfiles],cex=1,bty="n")
  }
}

