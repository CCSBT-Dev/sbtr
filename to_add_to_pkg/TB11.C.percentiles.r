plot.C.percentiles <- function(s3files,ylim=c(0.50),xlim=c(2011,2040), header, legend.lab, quants=c(0.1,0.5,0.9), first=T)
{
#Modified by TA Branch to allow for comparison of multiple CMPs and catch schedules. 
#Plot median biomass past and future projections for one MP and 
#one operating model.

#Arguments to function are:
#1) s4files: vector of output filename ending in .s4 from one of the projections, c("xxx.s4","yyy.s4")
#2) ylim: a vector giving the lower and upper bound to use for the y-axis 
#3) xlim: years to plot
#4) cpue.series: 0=c0, 1=c1, 2=c2, 3=c3 corresponding to different assumptions about how the cpue is 
#     affected by misreporting. 
#5) header: title to put on top of plot
#6) col.median: color of median line
#7) filename: if 1, uses the file in the tree, if a file is specified, uses that, e.g. filename="Cfull2_H60.s4"
#8) first: if T then plot y labels else don't

 .Options$warn <- -1  # suppress annoying warning messages
 min.yr <- 1931   #could loop backward through yr looking for length(grep(yr,colnames(tmp))==0, to automate this
 
 nfiles <- length(s3files)
 print(nfiles)
 if (nfiles==2) { plot.colors <- c("red","blue") }
 else { plot.colors <- rainbow(nfiles)  }
  for (i in 1:nfiles) {
   tmp <- read.table(s3files[i],skip=2,header=T)
   data.cols <- c(grep(paste("C.",xlim[1],sep=""),colnames(tmp)):grep(paste("C.",xlim[2],sep=""),colnames(tmp)))
   worms <- as.matrix(tmp[,data.cols])/1e+03

   quantiles<- apply(worms,2,quantile,quants)
   if (i>1) { par(new=T) }
   lwd=c(3,2,2,2,2,2,2,2,2,2,2)
   lty=c(1,2,2,2,2,2,2,2,2,2,2)
   for (j in 1:length(quants)) {
      if (j>1) {par(new=T)}
      if (length(quants)==1) {
         yvals <- quantiles
      }
      else {
         yvals <- quantiles[j,]
      }
      plot(xlim[1]:xlim[2], yvals,col=plot.colors[i],type="l",lwd=lwd[j],lty=lty[j],xlim=xlim,ylim=ylim,
                                                axes=F,xlab="",ylab="",yaxs="i",xaxs="i")
   }
 }
 box()
 axis(1,cex.axis=0.9)
 if (first==T) {   axis(2,las=1,cex.axis=0.9) }
 mtext(side=3,header,line=2,cex=1.3)
 if (first==T) {
    mtext(side=2,outer=F,expression(paste("Catch (",10^3," mt)",sep="")),line=3,cex=1.3)
 }
 mtext(side=1,outer=F,"Year",line=3,cex=1.3)
 if (!missing(legend.lab)) {
    legend(x=xlim[1],y=ylim[2],legend=legend.lab,col=plot.colors,bty="n",lty=1,lwd=2)
 }
 invisible(worms)
}
