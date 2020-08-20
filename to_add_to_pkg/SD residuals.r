#####################################################################
#plot histograms of SD of standardized residuals by data components.
#Coding by Trevor A. Branch and Ana Parma July 2009
#####################################################################
hist.SD.resids <- function(data.objects,label) {
  nobjects <- length(data.objects)
  resSD <- matrix(nrow=nobjects,ncol=8)
  
  for (i in 1:nobjects) {
    resSD[i,] <- t(data.objects[[i]]$res.stats[,1])
  }
  
  colnames(resSD)=c("LL1" ,"LL2" ,"LL3","LL4","Indo","Aus","CPUE","Tags")
    
  par(mfrow=c(4,2),mar=c(3,3,1,0),oma=c(1,2,4,1),mgp=c(1,1,0)) 

  for (i in 1:8)
  {
     yy = resSD[,i]
     nameplot = colnames(resSD)[i]
     hist(yy,main=nameplot,xlab="",ylab="",col="gray80",xaxs="i",yaxs="i",axes=F,las=1)
     axis(1,pos=0)
     axis(2,line=0)
  }
  mtext(paste("SD of standardized residuals",label),side=3,line=1,cex=1.2,outer=T)
  mtext("Frequency",side=2,line=0,cex=1.2,outer=T)
}
#pdf("figs\\SD resids.pdf",width=6,height=8)
#hist.SD.resids(data.objects=data.base5hsqrt, label="example")
#dev.off()
