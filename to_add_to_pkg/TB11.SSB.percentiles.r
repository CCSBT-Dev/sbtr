#' @title Plot SSB percentiles
#' @description
#' Plot median biomass past and future projections for one MP and one operating model. Modified by TA Branch to allow for comparison of multiple CMPs and catch schedules. 
#' @export
#' 
Plot.SSB.percentiles <- function(s4files,ylim,xlim=c(1985,2022), header, legend.lab, quants=c(0.1,0.5,0.9))
{
#Arguments to function are:
#1) s4files: vector of output filename ending in .s4 from one of the projections, c("xxx.s4","yyy.s4")
#2) ylim: a vector giving the lower and upper bound to use for the y-axis 
#3) xlim: years to plot
#4) cpue.series: 0=c0, 1=c1, 2=c2, 3=c3 corresponding to different assumptions about how the cpue is 
#     affected by misreporting. 
#5) header: title to put on top of plot
#6) col.median: color of median line
#7) filename: if 1, uses the file in the tree, if a file is specified, uses that, e.g. filename="Cfull2_H60.s4"

 .Options$warn <- -1  # suppress annoying warning messages
 par(mfrow=c(1,1),mar=c(4.5,4.5,4,1),oma=c(0,0,0,0))
 min.yr <- 1931   #could loop backward through yr looking for length(grep(yr,colnames(tmp))==0, to automate this
 
 nfiles <- length(s4files)
 if (nfiles==2) { plot.colors <- c("red","blue") }
 else { plot.colors <- rainbow(nfiles)  }
  for (i in 1:nfiles) {
   tmp <- read.table(s4files[i],skip=2,header=T)
   data.cols <- grep("B.",colnames(tmp),ignore.case=T)   #find all columns with "CPUE" in the name
   worms <- as.matrix(tmp[,data.cols])

   #data in file, and xlim may differ in years. Align them here.
   max.yr <- min.yr+length(data.cols)-1
   yr.first <- round(xlim[1],0)
   yr.last <- round(xlim[2],0)
   plot.cols <- match(yr.first:yr.last,min.yr:max.yr)   
   
   quantiles<- apply(worms[,plot.cols],2,quantile,quants)
   if (i>1) { par(new=T) }
   for (j in 1:NROW(quantiles)) {
      if (j>1) {par(new=T)}
       if (length(quants)==1) {
         yvals <- quantiles
      }
      else {
         yvals <- quantiles[j,]
      }
      if(quants[j]==0.1) { lty <- 2 }
      else {lty <- 1}
      plot(yr.first:yr.last, yvals/1e+06,col=plot.colors[i],type="l",lty=lty, lwd=2,xlim=xlim,ylim=ylim,
                                                axes=F,xlab="",ylab="",yaxs="i",xaxs="i")
   }
 }
 box()
 axis(1,cex.axis=0.9)
 axis(2,las=1,cex.axis=0.9)
 mtext(side=3,header,line=2,cex=1.3)
 mtext(side=2,outer=F,expression(paste("Spawning biomass ( ",10^6," mt)",sep="")),line=3,cex=1.3)
 mtext(side=1,outer=F,"Year",line=3,cex=1.3)
 if (!missing(legend.lab)) {
    legend(x=xlim[1],y=ylim[2],legend=legend.lab,col=plot.colors,bty="n",lty=1,lwd=2)
 }
 invisible(worms)
}
#source("TB10.SSB.medians.r")
#win.graph(height=10,width=10,pointsize=12)
#grid.files <- c("c1s1l13h","c1s1l1","c0s1l13h","c2s1l13h")
#s4files <- paste("CURRENT\\v0\\CURRENT_",grid.files,".s4",sep="")
#x <- plot.SSB.medians(s4files=s4files,ylim=c(0,1.2), xlim=c(1931,2035), 
#                     header="Spawning biomass",legend.lab=grid.files)
