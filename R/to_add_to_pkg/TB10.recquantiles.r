#' @title myboxes
#' 
myboxes <- function(x, width, stats, out = NULL, group, labels = 
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


#' @title recquantiles
#' @export
#' 
recquantiles <- function(s4files, xlim=c(1931,2032), ylim=c(0,1.4), plot.1989=T, caption="AAA",sub.titles) {
  nfiles <- length(s4files)

  par (mfrow =c(2,2),mai=c(0,0,0,0),omi=c(0.7,0.7,0.4,0.1),lwd=0.7)
  for (i in 1:nfiles) {
     qdat1<-read.table(s4files[i],skip=2,header=T)

     xquants<-c(0.1,0.25,0.5,0.75,0.90)
     par(col=1)

     #find recruitment years
     min.yr <- floor(xlim[2])
     max.yr <- ceiling(xlim[1])
     for (yr in ceiling(xlim[1]):floor(xlim[2])) {
        if(length(grep(paste("Rec.",yr,sep=""),colnames(qdat1)))>=1) {
           if (yr<min.yr) {min.yr <- yr}
           if (yr>max.yr) {max.yr <- yr}
        }
     }
     
     plot(1,1,yaxs="i",ylab="",xlab=" ",xlim=c(min.yr-1,max.yr+1),ylim=ylim,axes=F, xaxs="i",
                 type="n",cex.lab=0.85, cex.lab=1.1, cex.axis=0.8)
     if (!missing(sub.titles)) {
        text(x=mean(c(min.yr,max.yr)),y=0.95*ylim[2],sub.titles[i])
     }

     data.columns <- (grep(paste("Rec.",min.yr,sep=""),colnames(qdat1))):(grep(paste("Rec.",max.yr,sep=""),colnames(qdat1)) )
     #print(data.columns)
     qdat<-qdat1[,data.columns]/1e6
     ncolumns <- ncol(qdat)
     qtmp<-matrix(rep(NA,5*ncolumns),nrow=5)
     for (ix in 1:ncolumns)qtmp[,ix]<-unlist(quantile(qdat[,ix],xquants))
     myboxes(min.yr:max.yr,.25,qtmp,xcol="blue")
     if(i %in% 3:4) { axis(1) }
     if (i %in% c(1,3)) { axis(2,las=1) }
     box()
  }
  mtext(outer=T,line=2.5,side=1,"Year")  
  mtext(outer=T,line=2.8,side=2,"Recruitment (millions)")  
  mtext(outer=T,line=1.2,side=3,caption)
  invisible(qdat)
}
#Fig. 10 Plot boxplots for recruitment for a few scenarios
#source("TB10.recquantiles.r")
#win.graph(height=8,width=8,pointsize=12)
#s4files <- list()
#grid.files <- c("c1s1l13h","c1s1l1","c0s1l13h","c2s1l13h")
#s4files <- paste("CURRENT\\v0\\CURRENT_",grid.files,".s4",sep="")
#x <- recquantiles(s4files=s4files,ylim=c(0,18),xlim=c(1930,2033),caption="Recruitment under different scenarios",
#                  sub.titles=grid.files)
#x <- recquantiles(s4files=s4files,ylim=c(0,3.8),xlim=c(2000,2010),caption="Recruitment under different scenarios",
#                  sub.titles=grid.files)
