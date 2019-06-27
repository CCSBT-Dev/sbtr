#' @title myboxes
#' @description
#' Take the .s4 file and plot only the rows with h=1,2,3,4,5, etc. Easy to do since first item in each row is the code, which starts with 1,2,3,4,5
#'
myboxes<-function(x, width, stats, out = NULL, group, labels = 
 rep("", length = length(x)), horizontal = F,xcol=16)
{
 Segments <- if(horizontal) function(x1, y1, x2, 
   y2)
  segments(y1, x1, y2, x2) else function(
   x1, y1, x2, y2)
  segments(x1, y1, x2, y2)
 Points <- if(horizontal) function(x, y)
  points(y, x) else function(x, y)
  points(x, y)
 Mtext <- if(horizontal) function(name, x)
  mtext(name, 2, 2, at = x, srt = 90)
   else function(name, x)
  mtext(name, 1, 2, at = x, srt = 0)
 n <- length(x)
 if(length(width) == 1) {
  del <- rep(width, 5)
  del[c(1, 5)] <- width * 0.8
  del <- c( - del, del)
  xx <- outer(del, x, "+")
 }
 else {
  del <- matrix(width, 10, n, byrow = T)
  small <- c(1, 5, 6, 10)
  del[small,  ] <- 0.8 * del[small,  ]
  del[1:5,  ] <-  - del[1:5,  ]
  xx <- rep(x, rep(10, n)) + del
 }
 if(length(out) > 0) {
  Points(x[group], out)
 }
#whiskers, ticks at max & min, UQ and LQ
 for (inx in 1:length(xx[1,]))
 polygon(xx[c(2, 7, 7, 2),inx ], stats[c(2, 2, 4, 
  4),inx ], col = xcol)
 Segments(xx[1:5,  ], stats, xx[6:10,  ], stats)
 savlwd <- par("lwd")
 par(lwd = 2)
 Segments(xx[3,  ], stats[3,  ], xx[8,  ], stats[
  3,  ])
 par(lwd = savlwd) #sides of boxes
 Segments(xx[c(2, 7),  ], stats[c(2, 2),  ], xx[
  c(2, 7),  ], stats[c(4, 4),  ])
 savlty <- par("lty")
 par(lty = 2)
 xx <- rep(x, rep(2, n))
 Segments(xx, stats[c(5, 2),  ], xx, stats[c(4, 
  1),  ])
 par(lty = savlty)
 Mtext(labels, x)
 invisible(NULL)
}


#' @title SR quantiles by h
#' @description
#' Take the .s4 file and plot only the rows with h=1,2,3,4,5, etc. Easy to do since first item in each row is the code, which starts with 1,2,3,4,5
#' @export
#'
SRquantiles.by.h <- function(s4filename, xlim=c(1930.5,2032.5), R.ylim=c(0,100), B.ylim=c(0,1800), caption, h.level) {
   par (mfrow =c(2,1),mai=c(0,0.1,0,0),omi=c(0.9,1.0,0.5,0.1),lwd=0.7)

   qdat1<-read.table(s4filename,skip=2,header=T)
   B.columns <- grep("B.",colnames(qdat1))
   #print(h.level)
   h.rows <- (substr(qdat1[,2],1,1)==h.level)  #True for all rows where first number is equal to h.level
   #print(h.rows)   

   xquants<-c(0.1,0.25,0.5,0.75,0.90)
   par(col=1)

   ####Recruitment
   plot(1,1,yaxs="i",ylab="",xlab=" ",xlim=xlim,ylim=R.ylim,axes=F,
             type="n",cex.lab=0.85, cex.lab=1.1, cex.axis=0.8, las=1)
   R.columns <- grep("Rec.",colnames(qdat1))
   
   qdat<-qdat1[h.rows,R.columns]/1e6
   
   box()
   qtmp<-matrix(rep(NA,610),nrow=5)
   for (ix in 1:102)qtmp[,ix]<-unlist(quantile(qdat[,ix],xquants))
   myboxes(c(1931:2032),.25,qtmp,xcol="green")
   axis(2,las=2,cex.lab=1)
   mtext(outer=F,line=3.5,side=2,expression(paste("Recruitment (",10^6,")")),cex=1.1)  

   ####Spawning stock biomass
   plot(1,1,yaxs="i",ylab="",xlab=" ",xlim=xlim,ylim=B.ylim,
          type="n",cex.lab=0.85, cex=.8, cex.axis=0.85,xaxt="n",yaxt="n")
   axis(2,las=2,cex.lab=1)
   axis(1,cex.lab=1)
   
   qdat<-qdat1[h.rows,B.columns]/1e3
   
   box()
   #plot the line for B2004
   #B2004.column <- grep("B.2004",colnames(qdat1))
   #med.2004 <-median(qdat1[,B2004.column]/1e3) 
   #abline(h=med.2004,lty=1,lwd=2,col="red")
   #text(x=xlim[1]+1,y=med.2004,pos=3,expression(B[2004]),col="red",cex=1.2)

   #Plot the line for 20% of B0
   B0.20perc <-0.2*median(qdat1[h.rows,3]/1e3) 
   abline(h=B0.20perc,lty=1,lwd=2,col="orange")
   text(x=2010,y=B0.20perc,pos=3,expression(paste("20% of ",B[0])),col="orange",cex=1.2)

   #Plot the line for B1980
   #B1980.column <- grep("B.1980",colnames(qdat1))
   #B1980 <-median(qdat1[,B1980.column]/1e3) 
   #abline(h=B1980,lty=1,lwd=2,col="green")
   #text(x=xlim[2]-1,y=B1980,pos=3,expression(B[1980]),col="green",cex=1.2)

   qtmp<-matrix(rep(NA,610),nrow=5)
   for (ix in 1:102)qtmp[,ix]<-unlist(quantile(qdat[,ix],xquants))
   myboxes(c(1931:2032),.25,qtmp,xcol="gray50")

   mtext(outer=F,line=3.5,side=2,expression(paste("Spawning biomass (",10^3,"mt)")),cex=1.1)  
   mtext(outer=T,line=2.5,side=1,"Year",cex=1.1)  
   mtext(outer=T,line=0.5,side=3,caption, cex=1.2)  
   invisible(qdat1)
}
