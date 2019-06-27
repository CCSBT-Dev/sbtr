#' @title Plot to look at the trade-off between biomass and catch performance of MPs
#' @description
#' Plot median biomass past and future projections for one MP and one operating model. Modified by TA Branch to allow for comparison of multiple CMPs and catch schedules. 
#' @export
#' 
cpue.median.past.present.f <- function(s4files,ylim, legend.lab, 
             xlim=c(1985,2022), cpue.series = 1, header, col.median="red", cpue.file="cpue_for_plot2010.csv")
{
#Arguments to function are:
#1) s4filename: output filename ending in .s4 from one of the projections
#2) ylim: a vector giving the lower and upper bound to use for the y-axis 
#3) xlim: years to plot
#4) cpue.series: 0=c0, 1=c1, 2=c2, 3=c3 corresponding to different assumptions about how the cpue is 
#     affected by misreporting. 
#5) header: title to put on top of plot
#6) col.median: color of median line
#7) filename: if 1, uses the file in the tree, if a file is specified, uses that, e.g. filename="Cfull2_H60.s4"

  .Options$warn <- -1  # suppress annoying warning messages
  par(mfrow=c(1,1),mar=c(4.5,4.5,4,1),oma=c(0,0,0,0))

  CPUE.historical <- read.csv(cpue.file)
  npastCPUE <- nrow(CPUE.historical)
  last.yr.hist <- CPUE.historical[npastCPUE,1]   #final year of historical series

  nfiles <- length(s4files) 

  for (i in 1:nfiles) {
    tmp <- read.table(s4files[i],skip=2,header=T)
    cpue.cols <- grep("CPUE",colnames(tmp),ignore.case=T)   #find all columns with "CPUE" in the name
    cpueworms <- as.matrix(tmp[,cpue.cols])
    nfutureCPUE <- ncol(cpueworms)

    yr.first <- last.yr.hist+1
    yr.last <- xlim[2]

    CPUE.quantiles<- apply(cpueworms,2,median)
    n <- nrow(cpueworms)
    nquant<-nrow(CPUE.quantiles)
    CPUE.med <- CPUE.quantiles[1]

    #this needed to fill in gap between past and present
    CPUE.historical[npastCPUE+1,1] <- CPUE.historical[npastCPUE,1]+1  #the year after the historical cpue ends 
    CPUE.historical[npastCPUE+1,cpue.series[i]+2] <- CPUE.med

    #magically tells you which historical rows to plot based on xlim values
    #by finding all years in .csv that match the given years in the xlim
    historical.rows <- match(xlim[1]:xlim[2],CPUE.historical[,1])
    if (i>1) {  par(new=T)  }
    plot(CPUE.historical[historical.rows,1],CPUE.historical[historical.rows,cpue.series[i]+2],type="l",xlab="",ylab="",
                  axes=F, xlim=xlim,ylim=ylim,yaxs="i",xaxs="i", lwd=2,col="black")
    par(new=T)
    plot(CPUE.historical[historical.rows[npastCPUE:(npastCPUE+1)],1],
         CPUE.historical[historical.rows[npastCPUE:(npastCPUE+1)],cpue.series[i]+2],type="l",xlab="",ylab="",
                 axes=F, xlim=xlim,ylim=ylim,yaxs="i",xaxs="i", lwd=2,col="gray50")
    par(new=T)
    if(missing(ylim)) ylim <- c(0,max(CPUE.quantiles))
    plot.cols <- match(1:length(yr.first:yr.last),1:nfutureCPUE)

    lines(yr.first:yr.last,CPUE.quantiles[plot.cols],type="l",lwd=2,col=col.median[i])
    par(new=T)
    lines(yr.first:yr.last,CPUE.quantiles[plot.cols],type="p",pch=19,col=col.median[i])
  }
  box()   #because axes=F
  axis(1)
  axis(2,las=1)
  mtext(side=3,header,line=2,cex=1.3)
  mtext(side=2,outer=F,"CPUE",line=3,cex=1.3)
  mtext(side=1,outer=F,"Year",line=3,cex=1.3)
  if (!missing(legend.lab)) {
     legend(x=xlim[1]+0.2*diff(xlim),y=ylim[2],legend=legend.lab,col=col.median,bty="n",lty=1,lwd=2)
  }

  invisible(tmp)
}

#source("TB10.cpue.median.past.present.r")
#win.graph(height=10,width=10,pointsize=12)
#plot.cols <- rainbow(11)
#catch.levels <- seq(0,10000,1000)
#cpue.series <- rep(1,length(catch.levels))  #needed to get the right historical CPUE series
#s4files <- paste(".\\CONST\\v0\\CONST",catch.levels,"_c1s1l13h.s4",sep="")
#x <- cpue.median.past.present.f(s4files=s4files,ylim=c(0,3),xlim=c(1969,2035),
#             cpue.series=cpue.series, header=paste("CPUE for constant catch levels",gridname), col.median=plot.cols, 
#             cpue.file="cpue_for_plot2010.csv",legend.lab=paste(catch.levels,"mt"))
