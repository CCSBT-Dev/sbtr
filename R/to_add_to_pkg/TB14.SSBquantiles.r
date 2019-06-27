#' @title SSBquantiles
#' @export
#' 
SSBquantiles <- function(s4files, xlim=c(1931,2032),which.B=c("TB10plus","SSB")[1], 
                      ylim=c(0,1.6), plot.1989=T, caption="AAA",sub.titles) {
  nfiles <- length(s4files)

  par (mfrow =c(2,2),mai=c(0,0,0,0),omi=c(0.7,0.7,0.4,0.1),lwd=0.7)

  for (i in 1:nfiles) {
     qdat1<-read.table(s4files[i],skip=2,header=T)

     xquants<-c(0.1,0.25,0.5,0.75,0.90)
     par(col=1)


     #find data years
     min.yr <- floor(xlim[2])
     max.yr <- ceiling(xlim[1])
     for (yr in ceiling(xlim[1]):floor(xlim[2])) {
        if(length(grep(paste(which.B,yr,sep="."),colnames(qdat1)))>=1) {
           if (yr<min.yr) {min.yr <- yr}
           if (yr>max.yr) {max.yr <- yr}
        }
     }
     
     plot(1,1,yaxs="i",ylab="",xlab=" ",xlim=c(min.yr-1,max.yr+1),ylim=ylim,axes=F, xaxs="i",
                 type="n",cex.lab=0.85, cex.lab=1.1, cex.axis=0.8)
     if (!missing(sub.titles)) {
        text(x=mean(c(min.yr,max.yr)),y=0.95*ylim[2],sub.titles[i])
     }

     data.columns <- (grep(paste(which.B,min.yr,sep="."),colnames(qdat1))):(grep(paste(which.B,max.yr,sep="."),colnames(qdat1)) )
     #print(data.columns)
     qdat<-qdat1[,data.columns]/1e6
     ncolumns <- ncol(qdat)
     qtmp<-matrix(rep(NA,5*ncolumns),nrow=5)
     for (ix in 1:ncolumns)qtmp[,ix]<-unlist(quantile(qdat[,ix],xquants))
     myboxes(min.yr:max.yr,.25,qtmp,xcol="blue")
     if(i %in% 3:4) { axis(1) }
     if (i %in% c(1,3)) { axis(2,las=1) }
     if(i %in% 1:2  & nfiles <3) { axis(1) }

     box()
  }
  mtext(outer=T,line=2.5,side=1,"Year")  
  if (which.B == "SSB") 
  mtext(outer=T,line=2.8,side=2,expression(paste("Spawning biomass ( ",10^6," mt)",sep="")))
  if (which.B == "TB10plus") 
  mtext(outer=T,line=2.8,side=2,expression(paste("Biomass age 10+ ( ",10^6," mt)",sep="")))
  mtext(outer=T,line=1.2,side=3,caption)
  invisible(qdat)
}
#Fig. xx Plot boxplots for recruitment for a few scenarios
#source("TB14.SSBquantiles.r")
#win.graph(height=8,width=8,pointsize=12)
#s4files <- list()
#grid.files <- c("c1s1l13h","c1s1l1","c0s1l13h","c2s1l13h")
#s4files <- paste("CURRENT\\v0\\CURRENT_",grid.files,".s4",sep="")
#x <- SSBquantiles(s4files=s4files,which.B="TB10plus",ylim=c(0,0.42),xlim=c(1980,2021),caption="Spawning biomass by scenario",
#                  sub.titles=grid.files)
