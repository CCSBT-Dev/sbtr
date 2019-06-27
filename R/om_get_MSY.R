#' Plot histograms of MSY values.
#'
#' Get the MSY results from each cell of the grid to see what the distribution of values is. Trevor A. Branch, started 5 September 2009, Busan, South Korea.
#'
#' @export
#' 
get_MSY_vals <- function(data.objects, label)
{
  nobjects <- length(data.objects)
  
  var.names<- c("Scenario" ,"MSY" ,"Bmsy","B10msy","TBmsy","Fmsy","F", "F.Fmsy")
  MSY.data <- data.frame(matrix(nrow=nobjects,ncol=length(var.names)))
  names(MSY.data) <- var.names
  for (i in 1:nobjects) 
  {
    MSY.data$Scenario[i] <- data.objects[[i]]$scenario_number
    lengthMSY <- length(data.objects[[i]]$MSY)
    MSY.data$MSY[i] <- data.objects[[i]]$MSY[lengthMSY]
    lengthBmsy <- length(data.objects[[i]]$Bmsy)
    MSY.data$Bmsy[i] <- data.objects[[i]]$Bmsy[lengthBmsy]
    lengthBmsy <- length(data.objects[[i]]$Bmsy)
    lengthTBmsy <- length(data.objects[[i]]$TBmsy)
    MSY.data$TBmsy[i] <- data.objects[[i]]$TBmsy[lengthTBmsy]
    lengthFmsy_a215 <- length(data.objects[[i]]$Fmsy_a215)
    MSY.data$Fmsy[i] <- data.objects[[i]]$Fmsy_a215[lengthFmsy_a215]
    lengthF_a215 <- length(data.objects[[i]]$F_a215)
    MSY.data$F[i] <- data.objects[[i]]$F_a215[lengthF_a215]
    MSY.data$F.Fmsy[i] <- MSY.data$F[i]/MSY.data$Fmsy[i]
    lengthB <- nrow(data.objects[[i]]$Bmsy_a)
    MSY.data$B10msy[i] <- sum(data.objects[[i]]$Bmsy_a[lengthB,11:31])
  }
  
    
  par(mfrow=c(3,3),mar=c(3,3,1,0),oma=c(1,2,4,1),mgp=c(1,1,0)) 

  for (i in c(2:length(var.names)))
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

