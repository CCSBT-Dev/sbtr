###################SSB medians#########################################
#Given multiple s4 files, plots median SSB
#Trevor A. Branch tbranch@gmail.com
#####################################################################
med.rec.base <- function(s4files,B.ylim,xlim=c(1931,2031),leg.names=NULL,legend.x,legend.y)  {
  require(gplots)
  nfiles <- length(s4files)
  
  plot.colors <- rich.colors(nfiles)
  pch=c(1,2,4,15:23)
  for (i in 1:nfiles) {
    SSB.data <- read.table(s4files[i],skip=3)
    SSB.data <- as.matrix(SSB.data[,-c(1,2)])
    med.SSB <- apply(SSB.data[,106:206],MARGIN=2,quantile,0.5)/1e6
    #med.B20 <- 0.2*med.SSB[1]
    #med.B2004 <- med.SSB[2004-1931+1]
    if (i >1) {
      par(new=T)
    }
    
    plot(x=1931:2031,med.SSB,type="l",lwd=2,col=plot.colors[i],ylim=B.ylim,xlim=xlim,
                 yaxs="i",xaxs="i",xlab="",ylab="",axes=F)
    par(new=T)
    plot(x=1931:2031,med.SSB,type="p",pch=pch[i],col=plot.colors[i],ylim=B.ylim,xlim=xlim,
                 yaxs="i",xaxs="i",xlab="",ylab="",axes=F)
  }
  #abline(h=med.B20,lty=2,lwd=2,col="orange")
  #text(x=2020,y=med.B20+5,"20% of B0",col="orange")
  #abline(h=med.B2004,lty=2,lwd=2,col="red")
  #text(x=2002.5,y=med.B2004-5,"B2004",col="red")
  box()
  axis(2,las=1)
  mtext(side=2,"Recruitment (millions)",cex=1.3,line=3)
  mtext(side=3,"Base case",cex=1.6,line=0.5)
  legend(x=legend.x,y=legend.y,legend=leg.names,pch=pch,col=plot.colors,lwd=2,cex=1.2,bty="n")
}


###################SSB medians#########################################
#Given multiple s4 files, plots median SSB
#Trevor A. Branch tbranch@gmail.com
#####################################################################
med.SSB.base <- function(s4files,B.ylim,xlim=c(1931,2031),leg.names=NULL,legend.x,legend.y,Bmsy=NULL)  {
  require(gplots)
  nfiles <- length(s4files)
  
  plot.colors <- rich.colors(nfiles)
  pch=c(1,2,4,15:23)
  for (i in 1:nfiles) {
    SSB.data <- read.table(s4files[i],skip=3)
    SSB.data <- as.matrix(SSB.data[,-c(1,2)])
    
    med.SSB <- apply(SSB.data[,1:101],MARGIN=2,quantile,0.5)/1000
    med.B20 <- 0.2*med.SSB[1]
    #med.B2004 <- med.SSB[2004-1931+1]
    if (i >1) {
      par(new=T)
    }
    
    plot(x=1931:2031,med.SSB,type="l",lwd=2,col=plot.colors[i],ylim=B.ylim,xlim=xlim,
                 yaxs="i",xaxs="i",xlab="",ylab="",axes=F)
    par(new=T)
    plot(x=1931:2031,med.SSB,type="p",lwd=2,pch=pch[i],col=plot.colors[i],ylim=B.ylim,xlim=xlim,
                 yaxs="i",xaxs="i",xlab="",ylab="",axes=F)
  }
  abline(h=med.B20,lty=2,lwd=2,col="gray40")
  text(x=2005,y=med.B20*1.09,"20% of B0",col="gray40")
  if (!is.null(Bmsy)) {
     abline(h=Bmsy,lty=2,lwd=2,col="black")
     text(x=2010,y=Bmsy+5,"Bmsy",col="black")
  }
  #abline(h=med.B2004,lty=2,lwd=2,col="red")
  #text(x=2002.5,y=med.B2004-5,"B2004",col="red")
  box()
  axis(1)
  axis(2,las=1)
  mtext(side=1,"Year",cex=1.3,line=2.5)
  mtext(side=2,outer=F,"Spawning biomass (thousand mt)",cex=1.3,line=3)
}

