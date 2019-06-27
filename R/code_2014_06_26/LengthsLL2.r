############################################################################
#Fit of the model sbtmodxx.tpl to the data for southern bluefin tuna.
#Predicted length frequency for longline fishery 1 to the observed data.
#Required the output file _lab.rep.
#Requires library PBSmodelling, available from 
#Programmed by Trevor A. Branch 29 June 2009
#outputs from a _lab.rep file, assuming that the naming convention in the file has sections of 
#outputs separated by $name comments so that it can be easily parsed by readList() function of Jon Schnute 
#IMPORTANT: library PBSmodelling required, available at http://code.google.com/p/pbs-software/downloads/list
#then in R go to menu Packages, Install Package from Local zip file
#Run by:
###source("LengthsLL2.r")
##########################################################################
LengthsLL2 <- function(labrep.file="sbtmod22_lab.rep", case_label="c1s1l1orig.5_h1m1M1O1C2a1") {
   library(PBSmodelling)  #for readList
   library(gplots)        #for rich.colors()
   subtle.color <- "gray40"
   x <- readList(labrep.file)
   length.list <- seq(from=x$lengths[1],by=x$lengths[2],length.out=x$lengths[3])
   obs.data <- x$len.obs[x$len.obs[,1]==2,-c(1,2)]  #extract the lengths where the fishery is =1, and exclude the fishery and year
   pred.data <- x$len.pred[x$len.pred[,1]==2,-c(1,2)]
   years <- x$len.pred[x$len.pred[,1]==2,2]
   nyears <- length(years)
   nlengths <- length(length.list)
   mfcol <- c(ceiling(nyears/2),2)
   par(mfcol=mfcol,oma=c(3.8,4.5,3.5,1),mar=c(0,0,0,0))
   ylim <- c(0,1.05*max(obs.data,pred.data))
   cohort.color <- rich.colors(nlengths+5)[-c(1:5)]  #exclude the dark blues   
   
   #these used for drawing outside line on histogram
   xvals <- vector(length=2*nlengths)
   yvals <- vector(length=2*nlengths)
   for (yr in 1:nyears) { 
      names.arg <- rep("",nlengths)
      #histogram lines and bars the same color and no gaps between them
      x <- barplot(obs.data[yr,],space=0,ylim=ylim,las=1,names.arg=names.arg, cex.names=0.5, xaxs="i",yaxs="i",border=cohort.color,
                  col=cohort.color,axes=F,ylab="",xlab="")
      
      #do some fancy footwork to get top line of histogram drawn but not lines in between bins
      bin.width <- x[2]-x[1]
      bw2 <- bin.width/2
      for (i in 1:nlengths) {
         xvals[2*i-1] <- x[i]-bw2            
         xvals[2*i] <- x[i]+bw2
         yvals[2*i-1] <- obs.data[yr,i]
         yvals[2*i] <- obs.data[yr,i]
      }
      lines(xvals,yvals,col=subtle.color,lwd=0.5)
      
      #now plot the axes
      if (yr %% mfcol[1] == 0 || yr==nyears) {
         if (yr != nyears) {
            at.x <- x[-length(x)]
            lab.list <- length.list[-length(x)]
         }
         else {
            at.x <- x
            lab.list <- length.list
         }
         axis(side=1,at=at.x,lab=lab.list, line=-0.1,col.axis=subtle.color, col=subtle.color,lwd=0,lwd.ticks=0)  #just use for the labels, to allow more control than names.arg
      }
      if (yr <= mfcol[1]) {
        axis(2,las=1,at=c(0,0.2),col=subtle.color,col.axis=subtle.color,lwd=0.5)
      }
      par(new=T)
      par(xpd=NA)
      plot(x=x,y=pred.data[yr,],ylim=ylim, xlim=par("usr")[1:2], las=1,xaxs="i",yaxs="i",col="black",bg="white",pch=19,cex=1,axes=F,ylab="",xlab="")
      box(col=subtle.color,lwd=0.5)
      x.pos <- par("usr")[1] + 0.85*diff(par("usr")[1:2])   #par("usr") spits out the current coordinates of the plot window
      y.pos <- par("usr")[3] + 0.82*diff(par("usr")[3:4])   #par("usr") spits out the current coordinates of the plot window
      text(x=x.pos,y=y.pos,years[yr],cex=1.4, col=subtle.color)
      par(xpd=T)
   }
   mtext(side=1,outer=T,"Lengths",line=2.3)
   mtext(side=2,outer=T,"Proportions",line=3.2)
   mtext(side=3,outer=T,line=1.2,"Longline 2 length frequency")
   mtext(side=3,outer=T,line=0.2,paste("(",case_label,")",sep=""),cex=0.6)
}
#pdf("figs\\LengthsLL2.pdf",width=6, height=8)
#win.graph(width=7,height=11.5)
#LengthsLL2(labrep.file="sbtmod22_lab.rep",case_label="c1s1l1orig.5_h1m1M1O1C2a1")
#dev.off()





   
