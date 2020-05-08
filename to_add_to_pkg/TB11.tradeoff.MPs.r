#' @title Tradeoff MPs f
#' @export
#' 
tradeoff.MPs.f <- function(file.prefixes, file.text, xlim, ylim, plot.title="Plot comparing some stuff",
                  B.yr, B.rel="none", B.type=c("value", "ratio", "prob","ave"), B.mult=0.2, B.quant=0.5,
                  C.yr, C.rel="none", C.type=c("value", "ratio", "prob","ave"), C.mult=0.2, C.quant=0.5,
                  pch=NULL, col=NULL)
{
#Arguments to function are:
#file.prefixes: vector of prefixes of runs to be plotted (include file directories if needed)
#file.text:     vector of labels for each point plotted
#for median(B2022/(0.2B0)),         B.yr=2022, B.rel=1931, B.type="ratio", B.mult=0.2, B.quant=0.5
#for 10th percentile B2035,         B.yr=2035, B.rel="none", B.type="value", B.mult="none", B.quant=0.1
#for P(B2022>0.1B0),                B.yr=2022, B.rel=1931, B.type="prob", B.mult=0.1, B.quant="none"
#for 50th of ave B over 2015-2025   B.yr=2025, B.rel=2015, B.type="ave", B.mult="none", B.quant=0.5     
#similarly for catches.

	s3files <- paste(file.prefixes,".s3",sep="")
	s4files <- paste(file.prefixes,".s4",sep="")

	nfiles <- length(file.prefixes) 

	par(mfrow=c(1,1),mai=c(0,0,0,0),omi=c(0.8,1.1,0.5,0.1))
	par(cex.axis=.9,cex.main=1)

	B.data <- vector(length=nfiles)
	C.data <- vector(length=nfiles)
	for (i in 1:nfiles)	 {
		tmp3 <- read.table(s3files[i],skip=2,header=T)
		tmp3 <- as.matrix(tmp3[,-c(1,2)])
		C.cols <- grep("C.",colnames(tmp3))
		cworms <- tmp3[,C.cols]
		tmp4 <- read.table(s4files[i],skip=2,header=T)
		tmp4 <- as.matrix(tmp4[,-c(1,2)])
		B.cols <- grep("B.",colnames(tmp4))
		bworms <- tmp4[,B.cols]

      B.axis.text="Biomass placeholder"
      C.axis.text="Catch placeholder"
      if (B.type=="value") {
		   B.yr.col <- grep(paste("B.",B.yr,sep=""),colnames(tmp4))
         B.data[i] <- quantile(tmp4[,B.yr.col],probs=B.quant)/1e06
         B.axis.text=paste(B.quant*100,"th B",B.yr," (million t)",sep="")
         #print(paste("B",B.data[i],titles.base[i]))
      }

      if (B.type=="ratio") {
		   B.yr.cols <- c(grep(paste("B.",B.yr,sep=""),colnames(tmp4)), grep(paste("B.",B.rel,sep=""),colnames(tmp4)))
         B.data[i] <- quantile(tmp4[,B.yr.cols[1]]/tmp4[,B.yr.cols[2]],probs=B.quant)
         B.axis.text=paste(B.quant*100,"th B",B.yr,"/B",B.rel,sep="")
         print(B.axis.text)
      }

      if (C.type=="value") {
		   C.yr.col <- grep(paste("C.",C.yr,sep=""),colnames(tmp3))
         C.data[i] <- quantile(tmp3[,C.yr.col],probs=C.quant)/1e03
         C.axis.text=paste(C.quant*100,"th C",C.yr," (thousand t)", sep="")
         #print(paste("C",C.data[i],titles.base[i]))
      }
      
      if (C.type=="ratio") {
		   C.yr.cols <- c(grep(paste("C.",C.yr,sep=""),colnames(tmp4)), grep(paste("C.",C.rel,sep=""),colnames(tmp4)))
         C.data[i] <- quantile(tmp4[,C.yr.cols[1]]/tmp4[,C.yr.cols[2]],probs=C.quant)
         C.axis.text=paste(C.quant*100,"th C",C.yr,"/C",C.rel,sep="")
         print(C.axis.text)
      }

   }
	if(missing(pch)) {   pch=nfiles:1  }
	else {pch=pch}
	if(missing(col)) {   col=1:nfiles  }
	else {col=col}

	if(missing(xlim)) xlim <- c(.975*min(B.data),1.025*max(B.data))
	if(missing(ylim)) ylim <- c(.975*min(C.data),1.025*max(C.data))
	
	plot(B.data,C.data,xlim=xlim,ylim=ylim,las=1,type="n", axes=F)
	box()
   points(B.data,C.data,pch=pch,col=col,lwd=2,cex=2)
   
	axis(1)
	axis(2,las=1)

   legend(xlim[1],ylim[1]+diff(ylim)*0.5,file.text,pch=pch,col=col)
   mtext(outer=T,side=1,B.axis.text,line=2.5)
   mtext(outer=T,side=2,C.axis.text,line=2.5)
   mtext(outer=T,side=3,plot.title,line=1)
}

#file.prefixes <-  c("MP3_2030_3000_inc\\MP3_2030_3000_inc_base",
#                    "MP3_2030_3000_noinc\\MP3_2030_3000_noinc_base",
#                    "MP3_2035_3000_inc\\MP3_2035_3000_inc_base",
#                    "MP3_2035_3000_noinc\\MP3_2035_3000_noinc_base",
#                    "MP3_2035_5000_inc\\MP3_2035_5000_inc_base",
#                    "MP3_2035_5000_noinc\\MP3_2035_5000_noinc_base",
#                    "MP3_2040_3000_inc\\MP3_2040_3000_inc_base",
#                    "MP3_2040_3000_noinc\\MP3_2040_3000_noinc_base")
#titles.base <- c("2030 3000 inc","2030 3000 noinc","2035 3000 inc","2035 3000 noinc","2035 5000 inc","2035 5000 noinc","2040 3000 inc","2040 3000 noinc") 
#tradeoff.MPs.f(file.prefixes=file.prefixes, file.text=titles.base, plot.title="Tradeoff: median C2022 vs. median B2022",
#                  B.yr=2022, B.type=c("value"), B.quant=0.5,
#                  C.yr=2022, C.type=c("value"), C.quant=0.5, ylim=c(14,21))
#
