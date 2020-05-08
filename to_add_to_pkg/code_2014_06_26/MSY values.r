################################################################################
#Get the MSY results from each cell of the grid to see what the distribution
#of values is.
#
#
#Trevor A. Branch, started 5 September 2009, Busan, South Korea.
################################################################################


#####################################################################
#plot histograms of MSY values
#Trevor A. Branch 
#####################################################################
MSY.vals <- function(data.objects,label) {
  nobjects <- length(data.objects)
  MSY.data <- matrix(nrow=nobjects,ncol=7)
  
  for (i in 1:nobjects) {
    MSY.data[i,1] <- data.objects[[i]]$scenario_number
    lengthMSY <- length(data.objects[[i]]$MSY)
    MSY.data[i,2] <- data.objects[[i]]$MSY[lengthMSY]
    lengthBmsy <- length(data.objects[[i]]$Bmsy)
    MSY.data[i,3] <- data.objects[[i]]$Bmsy[lengthBmsy]
    lengthTBmsy <- length(data.objects[[i]]$TBmsy)
    MSY.data[i,4] <- data.objects[[i]]$TBmsy[lengthTBmsy]
    lengthFmsy_a215 <- length(data.objects[[i]]$Fmsy_a215)
    MSY.data[i,5] <- data.objects[[i]]$Fmsy_a215[lengthFmsy_a215]
    lengthF_a215 <- length(data.objects[[i]]$F_a215)
    MSY.data[i,6] <- data.objects[[i]]$F_a215[lengthF_a215]
    MSY.data[i,7] <- MSY.data[i,6]/MSY.data[i,5]
  }
  
  colnames(MSY.data)=c("Scenario" ,"MSY" ,"Bmsy","TBmsy","Fmsy","F", "F/Fmsy")
    
  par(mfrow=c(2,3),mar=c(3,3,1,0),oma=c(1,2,4,1),mgp=c(1,1,0)) 

  for (i in c(2:7))
  {
     yy = MSY.data[,i]
     nameplot = colnames(MSY.data)[i]
     hist(yy,main=nameplot,xlab="",ylab="",col="gray80",xaxs="i",yaxs="i",axes=F,las=1)
     axis(1,pos=0)
     axis(2,line=0)
  }
  mtext(paste("MSY information",label),side=3,line=1,cex=1.2,outer=T)
  mtext("Frequency",side=2,line=0,cex=1.2,outer=T)
  invisible(MSY.data)
}
#source("get.all.files.r")
#baseMSY <- get.all.files(directory="arc\\baseflexLL3MSY")
#pdf(file="figs\\MSY.pdf",width=10,height=8)
#MSY.vals(data.objects=baseMSY,label="")
#dev.off()

