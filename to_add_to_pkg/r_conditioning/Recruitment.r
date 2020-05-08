###############################################
#Recruitment time series with standard errors.
#In the original Ianelli viewer, this was obtained from the sbtmodXX.std file. 
#The .std file, everything starting with "Recs" contains the recruitment data, first column means, second column SE.
#Then CV = SE/Mean
#And the lower CI is given by mean / exp(2*sqrt(log(1+se^2/mean^2)))
#The upper CI is given by mean * exp(2*sqrt(log(1+se^2/mean^2)))
#Notes: assumes that only the lines with recruitment estimates contain the text "Recs"
####source("Recruitment.r")
##########################################################################
plot.recruitment <- function(std.file="sbtmod22.std", labrep.file="sbtmod22_lab.rep",case_label="c1s1l1orig.5_h1m1M1O1C2a1") {
   #get the start and end years from the labrep file
   library(PBSmodelling)
   x <- readList(labrep.file)
   start.yr <- x$years[1]
   end.yr <- x$years[2] + 1

   #read in recruitment and SE of recruitment from the .std file
   x <- read.fwf(file=std.file,widths=c(7,21,12,11),skip=1,colClasses=c("numeric","character","numeric","numeric"))
   #use grep to find row numbers that contain "Recs" since the row labels contain lots of spaces
   rec.pos <- grep("Recs",x[,2])
   mean.rec <- x[rec.pos,3]/1000000
   se.rec <- x[rec.pos,4]/1000000

   #start with the plotting thing
   par(mfrow=c(1,1),oma=c(0,0,1,0),mar=c(4,4,1,1))
   nyears <- length(rec.pos)
   year.labs <- start.yr:end.yr
   
   #calculate the lognormal 95% confidence intervals for plotting
   lowerCI <- mean.rec/exp(2*sqrt(log(1+se.rec*se.rec/(mean.rec*mean.rec))))
   upperCI <- mean.rec*exp(2*sqrt(log(1+se.rec*se.rec/(mean.rec*mean.rec))))
   
   ylim <- c(0,1.05*max(mean.rec,upperCI))
   xlim <- c(min(year.labs)-1,max(year.labs)+1)
   plot.colors <- c(rep("black",nyears-1),"gray50")
   plot(x=year.labs,y=mean.rec,ylim=ylim,xlim=xlim,xaxs="i",yaxs="i",ylab="",xlab="",las=1,axes=F,type="p",pch=19,col=plot.colors,cex=1.1)
   arrows(x0=year.labs,x1=year.labs,y0=lowerCI,y1=upperCI,code=0,col=plot.colors)
   box()
   axis(side=1,at=seq(1930,2010,10))
   axis(side=2,at=pretty(ylim),las=1)
   mtext(side=1,"Year",line=2.5,cex=1.3)
   mtext(side=2,"Age-0 recruitment (millions)",line=2.5,cex=1.3)
   mtext(side=3,outer=T,line=-0.2,case_label)
   
   return(cbind(mean.rec,se.rec,lowerCI,upperCI))
}
#pdf(file="figs\\Recruitment v3.pdf",width=8,height=8)
#win.graph(width=8,height=8)
#x <- plot.recruitment(std.file="sbtmod22.std", labrep.file="sbtmod22_lab.rep",case_label="c1s1l1orig.5_h1m1M1O1C2a1")
#dev.off()

