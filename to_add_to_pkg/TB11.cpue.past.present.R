
cpue.past.present.f <- function(s4filename,ylim=c(0,4),xlim=c(1969,2022), cpue.series = 1, 
                        header, cpue.file="cpue_for_plot2010.csv")
{
#Function to plot the nice CPUE shaded plots, developed by Jason Hartog (I think).
#Modified by TA Branch to allow for comparison of multiple CMPs and catch schedules. 
#Function to plot median biomass and catch projections for one MP and 
#one operating model, as well as the 80% confidence interval;
#can also include any number of individual realizations (i.e. worms)

#Arguments to function are:
#1) dr: a string specifying the decision rule to be plotted; eg. "CON_00"
#2) ver: a string specifying the version (including TAC-interval option,
#     "a" for 1-yr or "b" for 3-yrs or "c" for 5-yrs);  eg."1a"
#3) model: a string specifying the operating model (default="Cfull2")
#4) yaxis: a vector giving the lower and upper bound to use for the y-axis of the 
#     CPUE plot (eg. c(0,10)); if missing, the program automatically calculates bounds
#5)filename: if 1, uses the file in the tree, if a file is specified, uses that, e.g. filename="Cfull2_H60.s4"

 .Options$warn <- -1  # suppress annoying warning messages
 
 par(cex.lab=1.2)
 
 CPUE.historical <- read.csv(cpue.file)
 nCPUE <- nrow(CPUE.historical)
 yr.first <- CPUE.historical[nCPUE,1] +1  #the first year in the projected CPUE series
 yr.last <- round(xlim[2],0)

 tmp <- read.table(s4filename,skip=2,header=T)
 cpue.col.first <- grep(paste("CPUE.",yr.first,sep=""),colnames(tmp))   #auto-find the column names with the CPUE data
 cpue.col.last <- grep(paste("CPUE.",yr.last,sep=""),colnames(tmp))   #auto-find the column names with the CPUE data
 cpueworms <- as.matrix(tmp[,cpue.col.first:cpue.col.last])
 
 CPUE.quantiles<- apply(cpueworms,2,function(x){quantile(x,seq(.05,.95,.05))})
 n <- nrow(cpueworms)
 nquant<-nrow(CPUE.quantiles)
 CPUE.med <- CPUE.quantiles[(nquant+1)/2,1]

 if(missing(ylim)) ylim <- c(0,max(CPUE.quantiles))
 else ylim <- ylim
 
 #plot the historical CPUE
 plot(CPUE.historical[,1],CPUE.historical[,cpue.series+2],type="l",xlab="Year",ylab="CPUE (value)",
                     xlim=xlim,ylim=ylim,yaxs="i", xaxs="i", lwd=3, axes=T,las=1)
 #connection between historical and present
 CPUE.historical[nCPUE+1,1] <- yr.first
 CPUE.historical[nCPUE+1,cpue.series+2] <- CPUE.med
 par(new=T)
 plot(CPUE.historical[nCPUE:(nCPUE+1),1],CPUE.historical[nCPUE:(nCPUE+1),cpue.series+2],type="l",lty=2,xlab="Year",ylab="CPUE (value)",
                     xlim=xlim,ylim=ylim,yaxs="i", xaxs="i", lwd=2, axes=T,las=1)

 #shaded area
 for(i in 1:((nquant-1)/2)){
     polygon(c(yr.first:yr.last,yr.last:yr.first),c(CPUE.quantiles[i,],
            rev(CPUE.quantiles[nquant-i+1,])),
            col=paste("grey",round(seq(90,5,-90*2/nquant))[i],sep=""),border=NA)
 } 
 #white circles and line in median
 par(new=T)
 plot(yr.first:yr.last,CPUE.quantiles[(nquant+1)/2,],type="l",lwd=2,col="white",xlim=xlim,ylim=ylim,yaxs="i", 
            xaxs="i",axes=F, xlab="",ylab="")
 par(new=T)
 plot(yr.first:yr.last,CPUE.quantiles[(nquant+1)/2,],type="p",pch=19,col="white",xlim=xlim,ylim=ylim,yaxs="i", 
            xaxs="i",axes=F, xlab="",ylab="")

 box()
 mtext(side=3,header,line=2,cex=1.3)
 invisible(cpueworms)
}
#source("TB10.cpue.past.present.R")
#win.graph(height=8,width=10)
#grid.file <- "c1s1l13h"
#s4file <- paste("CURRENT\\v0\\CURRENT_",grid.file,".s4",sep="") 
#x <- cpue.past.present.f(s4filename=s4file,ylim=c(0,2.4),cpue.series=1,header=grid.file,xlim=c(1969,2025))
