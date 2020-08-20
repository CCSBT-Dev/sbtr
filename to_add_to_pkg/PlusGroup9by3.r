################################################################################
#
#IMPORTANT: library PBSmodelling required, available at http://code.google.com/p/pbs-software/downloads/list
#then in R go to menu Packages, Install Package from Local zip file
#Programmed by Trevor A. Branch 13 July 2009
#Run by:
###source("PlusGroup7by3.r")
################################################################################

###############################
#####NumbersAtAge
###############################
NumbersAtAge <- function(xx, min.age=8,plot.years=c(1931,2008)) {
   x <- xx
   numbers <- x$Ns   #numbers at age by year, rows are years, columns are ages

   ages <- x$ages    #min and max age
   ages.list <- ages[1]:ages[2]
   nages <- length(ages.list)

   years <- x$years  #start and end years of the model
   nyears <- length(plot.years)

   mfcol <- c(nyears,1)
   plot.ages <- ages.list[ages.list>=min.age]
   subtle.color <- "black"
   cohort.color <- rich.colors(length(plot.ages))
   
   for (yr in 1:nyears) {
      names.arg <- rep("",sum(ages.list>=min.age))
      ylim <- c(0,1.05*max(numbers[plot.years[yr]-years[1]+1,ages.list>=min.age]/1e06))
      x <- barplot(numbers[plot.years[yr]-years[1]+1,ages.list>=min.age]/1e06,space=0,las=1,names.arg=names.arg, ylim=ylim,cex.names=0.5,yaxs="i",
                        col=cohort.color, axes=F,ylab="",xlab="", border="gray40",lwd=0.5)
      box(col=subtle.color,lwd=0.5)

   }
   #mtext(side=1,outer=F,"Age",line=3,cex=1.1)
   return(numbers)
}

################################
####Spawning biomass
################################
PlotSSB <- function(xx, ylim=NULL) {
   x <- xx
   SSB <- x$Sbio
   years <- x$years  #start and end years of the model
   years.list <- c(years[1]:years[2],years[2]+1)

   if(is.null(ylim)) {
      ylim <- c(0,1.1*max(SSB))
   }

   plot(x=years.list,y=SSB/1e6,ylim=ylim/1e6,yaxs="i",type="l",lwd=2.5,las=1,ylab="",xlab="Year",col="black")
   #plot the line for B2004
   med.2004 <-SSB[2004-years[1]+1]/1e6 
   abline(h=med.2004,lty=1,lwd=2,col="red")

   #Plot the line for 20% of B0
   B0.20perc <-0.2*(SSB[1]/1e6) 
   abline(h=B0.20perc,lty=1,lwd=2,col="orange")

   #Plot the line for B1980
   B1980 <-SSB[1980-years[1]+1]/1e6 
   abline(h=B1980,lty=1,lwd=2,col="green")
   
   legend(x=1960,y=0.99*ylim[2]/1e06,bty="n",lty=1,lwd=2,col=c("black","green","orange","red"),
                  legend=c("Spawning biomass",expression(B[1980]),expression(paste("20% of ",B[0])),expression(B[2004])))
   mtext(side=2,outer=F,"Spawning biomass (million mt)",line=3,cex=1.1)
}
#PlotSSB(labrep.file="sbtmod22\\arc\\C1S1L1orig.5\\C1S1L1orig.5_h1m1M1O1C2a1_lab.rep")

################################
####Indonesian selectivity
################################
Indonesian.selectivity <- function(xx,year.list=c(1952,seq(1994,2010,2)),do.legend=F) {
   x <- xx
   Ind.sel <- x$sel[x$sel[,1]==5,]
   nyears <- length(year.list)
   cohort.color <- c("black",rich.colors(nyears-1))
   #ylim <- c(0,1.05*max(Ind.sel[,-c(1:2)]))
   ylim <- c(0,4.9)
   for (yr in 1:nyears) {
      plot(x=6:30,y=Ind.sel[Ind.sel[,2]==year.list[yr],-c(1,2,3:8)],col=cohort.color[yr],lty=1,type="l",ylim=ylim,lwd=c(1.8),las=1,ylab="",xlab="",axes=F,yaxs="i",xaxs="i")
      par(new=T) 
   }
   plot(x=6:30,y=Ind.sel[Ind.sel[,2]==year.list[1],-c(1,2,3:8)],col="black",lty=1,type="l",ylim=ylim,lwd=c(1.8),las=1,ylab="",xlab="",axes=F,yaxs="i",xaxs="i")
   #axis(1)
   #
   box()

   if (do.legend==T) { 
      legend(x=6,y=ylim[2],bty="n",lty=1,lwd=2,col=cohort.color,legend=year.list,cex=0.8)
   }
}
#Indonesian.selectivity(labrep.file="sbtmod22_lab.rep")

################################
####M vector plotting
################################
Plot.M.vector <- function(xx) {
   x <- xx
   ages <- x$ages    #min and max age
   M.vector <- x$M
   #ylim <- c(0,1.05*max(M.vector))
   ylim <- c(0,0.54)
   par(xpd=T)
   plot(x=6:30,y=M.vector[7:31],col="black",lwd=1.2,type="l",pch=21,ylim=ylim,las=1,ylab="",xlab="",axes=F,yaxs="i",xaxs="i")
   par(new=T)
   plot(x=6:30,y=M.vector[7:31],col="black",bg="black",cex=1.1,type="p",pch=21,ylim=ylim,las=1,ylab="",xlab="",axes=F,yaxs="i",xaxs="i")
   par(xpd=F)
   box()
   axis(1)
}
#Plot.M.vector(labrep.file="sbtmod22_lab.rep")


################################################################################
#Compare plus groups
#Compare a variety of different model runs in terms of model numbers at age, 
#Indonesian selectivity and natural mortality by age schedules.
################################################################################
compare.plus.groups2 <- function(compare.objects,ylim=c(0,1),
                                 label) {
    library(gplots)
    nfiles <- length(compare.objects) 
    par(mfcol=c(3,nfiles),oma=c(5,5,3,5),mar=c(0,0,0,0))
    for (i in 1:nfiles)
    {
      xx<-compare.objects[[i]]
      NumbersAtAge(xx=xx, min.age=6,plot.years=c(2008))
      if (i==1) { 
         mtext(side=2,outer=F,"2008 numbers (relative)",line=3.5,cex=1.1)  
         #axis(2,las=1,col="black", col.axis="black",lwd=0.5)
      }
      #mtext(side=3,outer=F,line=1,files[i],cex=0.8)
      if(i==nfiles) {
         do.legend<-T
      }
      else {
         do.legend<-F
      }
      Indonesian.selectivity(xx=xx,do.legend=do.legend)
      if (i==1) {
         axis(2,las=1)
         mtext(side=2,outer=F,"Selectivity",line=3.5,cex=1.1)
      }
      #par(new=T)
      Plot.M.vector(xx=xx)
      if(i==1) {
         axis(2,las=1)
         mtext(side=2,outer=F,line=3.5,"Natural mortality",cex=1.1)
      }
    }
    mtext(side=3,outer=T,line=0.8,
          paste("Plus group diagnostics",label),
          cex=1.3)
    mtext(side=1,outer=T,"Age",line=3,cex=1.1)
}
#pdf("figs\\previous-smoothsqrt.pdf",width=11.5,height=6)
#compare.plus.groups2(directory="previous-smoothsqrt",ylim=c(0,1))
#dev.off()
