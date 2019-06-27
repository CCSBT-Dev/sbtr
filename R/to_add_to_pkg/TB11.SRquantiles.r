#' @title SR quantiles
#' @export
#'
SRquantiles <- function(s4filename, years=c(1931,2042), R.ylim=c(0,100), B.ylim=c(0,1800), caption) {
   year.vec <- years[1]:years[2]
   nyears <- length(year.vec)
   par (mfrow =c(2,1),mai=c(0,0.1,0,0),omi=c(0.9,1.0,0.5,0.1),lwd=0.7)

   qdat1<-read.table(s4filename,skip=2,header=T)
   B.columns <- (grep(paste("B.",years[1],sep=""),colnames(qdat1))):(grep(paste("B.",years[2],sep=""),colnames(qdat1)))
   R.columns <- (grep(paste("Rec.",years[1],sep=""),colnames(qdat1))):(grep(paste("Rec.",years[2],sep=""),colnames(qdat1)))

   xquants<-c(0.1,0.25,0.5,0.75,0.90)
   par(col=1)

   ####Recruitment
   plot(1,1,yaxs="i",ylab="",xlab=" ",xlim=years,ylim=R.ylim,axes=F,
             type="n",cex.lab=0.85, cex.lab=1.1, cex.axis=0.8, las=1)
   
   qdat<-qdat1[,R.columns]/1e6
   box()
   qtmp<-matrix(rep(NA,5*nyears),nrow=5)
   for (ix in 1:nyears)qtmp[,ix]<-unlist(quantile(qdat[,ix],xquants))
   myboxes(year.vec,.25,qtmp,xcol="green")
   axis(2,las=2,cex.lab=1)
   mtext(outer=F,line=3.5,side=2,expression(paste("Recruitment (",10^6,")")),cex=1.1)  

   ####Spawning stock biomass
   plot(1,1,yaxs="i",ylab="",xlab=" ",xlim=years,ylim=B.ylim,
          type="n",cex.lab=0.85, cex=.8, cex.axis=0.85,xaxt="n",yaxt="n")
   axis(2,las=2,cex.lab=1)
   axis(1,cex.lab=1)
   qdat<-qdat1[,B.columns]/1e3
   box()
   #plot the line for B2004
   B2004.column <- grep("B.2004",colnames(qdat1))
   med.2004 <-median(qdat1[,B2004.column]/1e3) 
   abline(h=med.2004,lty=1,lwd=2,col="red")
   text(x=years[1]+2,y=med.2004,pos=3,expression(B[2004]),col="red",cex=1.2)

   #Plot the line for 20% of B0
   B0.20perc <-0.2*median(qdat1[,3]/1e3) 
   abline(h=B0.20perc,lty=1,lwd=2,col="orange")
   text(x=1995,y=B0.20perc,pos=3,expression(paste("20% of ",B[0])),col="orange",cex=1.2)

   #Plot the line for B1980
   #B1980.column <- grep("B.1980",colnames(qdat1))
   #B1980 <-median(qdat1[,B1980.column]/1e3) 
   #abline(h=B1980,lty=1,lwd=2,col="green")
   #text(x=1945,y=B1980,pos=3,expression(B[1980]),col="green",cex=1.2)

   qtmp<-matrix(rep(NA,5*nyears),nrow=5)
   for (ix in 1:nyears)qtmp[,ix]<-unlist(quantile(qdat[,ix],xquants))
   myboxes(year.vec,.25,qtmp,xcol="gray50")

   mtext(outer=F,line=3.5,side=2,expression(paste("Spawning biomass (",10^3,"mt)")),cex=1.1)  
   mtext(outer=T,line=2.5,side=1,"Year",cex=1.1)  
   mtext(outer=T,line=0.5,side=3,caption, cex=1.2)  
   invisible(qdat1)
}
##pdf("Figs\\Fig 71 SRquantiles base.pdf",height=8,width=6)
##SRquantiles(s4filename="MP3_2035_3000_noinc\\MP3_2035_3000_noinc_base.s4", years=c(1931,2010), R.ylim=c(0,16), B.ylim=c(0,1190), caption="") 
##dev.off()



