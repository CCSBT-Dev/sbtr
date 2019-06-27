#' Plot median biomass past and future projections for one MP and one operating model.
#' 
#' Modified by TA Branch to allow for comparison of multiple CMPs and catch schedules.
#' 
#' @param s4files vector of output filename ending in .s4 from one of the projections, c("xxx.s4","yyy.s4")
#' @param ylim a vector giving the lower and upper bound to use for the y-axis 
#' @param xlim years to plot
#' @param cpue.series 0=c0, 1=c1, 2=c2, 3=c3 corresponding to different assumptions about how the cpue is affected by misreporting. 
#' @param header title to put on top of plot
#' @param col.median color of median line
#' @param filename if 1, uses the file in the tree, if a file is specified, uses that, e.g. filename="Cfull2_H60.s4"
#' @export
#' 
Plot.C.medians <- function(s3files,ylim=c(0.50),xlim=c(2011,2040), header, legend.lab)
{
 .Options$warn <- -1  # suppress annoying warning messages
 par(mfrow=c(1,1),mar=c(4.5,4.5,4,1),oma=c(0,0,0,0))
 min.yr <- 1931   #could loop backward through yr looking for length(grep(yr,colnames(tmp))==0, to automate this
 
 nfiles <- length(s3files)
 if (nfiles==2) { plot.colors <- c("red","blue") }
 else { plot.colors <- rainbow(nfiles)  }
  for (i in 1:nfiles) {
   tmp <- read.table(s3files[i],skip=2,header=T)
   data.cols <- c(grep(paste("C.",xlim[1],sep=""),colnames(tmp)):grep(paste("C.",xlim[2],sep=""),colnames(tmp)))
   worms <- as.matrix(tmp[,data.cols])/1e+03

   plot.cols <- data.cols
   
   medians <- apply(worms,2,median)
   if (i>1) { par(new=T) }
   plot(xlim[1]:xlim[2], medians,col=plot.colors[i],type="l",lwd=2,xlim=xlim,ylim=ylim,
                                                axes=F,xlab="",ylab="",yaxs="i",xaxs="i")
 }
 box()
 axis(1,cex.axis=0.9)
 axis(2,las=1,cex.axis=0.9)
 mtext(side=3,header,line=2,cex=1.3)
 mtext(side=2,outer=F,expression(paste("Catch ( ",10^3," mt)",sep="")),line=3,cex=1.3)
 mtext(side=1,outer=F,"Year",line=3,cex=1.3)
 if (!missing(legend.lab)) {
    legend(x=xlim[1],y=ylim[2],legend=legend.lab,col=plot.colors,bty="n",lty=1,lwd=2)
 }
 invisible(worms)
}
