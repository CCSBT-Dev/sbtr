#' Plots boxplots of spawning biomass ratios
#'
#' @param  ylims a list of set of lims
#' @export
#' 
Bratio.boxplots <- function (s4files, title = "", scenario.descr = "", xlab = "Scenario", ylims, ...)
{
 #B2006,  B2006/B0, B2014/2004, B2022:B2004
 #can handle multiple numbers of files
 #win.graph(height=8,width=3,pointsize=14)
 #boxplots(list("cfullorig.5.s4","cfullsqrt.s4"))
 
 boxwex <- 0.7
 
 nfiles <- length(s4files)
 par(mfrow=c(2,2),oma=c(5,0,2,0),mar=c(0,4,1,0.5),lwd=0.7)
 
 BdataList=list()

 for (i in 1:nfiles)
 {
  qdat1<-read.table(s4files[i],skip=2,header=T)
  B.columns <- grep("B.",colnames(qdat1))
  BdataList[[i]]<-qdat1[,B.columns]
 }

# Bdepletion <- list()
# for (i in 1:nfiles)  {
#   Bcurrent.column <- grep("B.2011",colnames(BdataList[[i]])) 
#   Bdepletion[[i]] <- (BdataList[[i]])[,Bcurrent.column]/1e+03
# }
# boxplot(Bdepletion,names=rep("",nfiles),boxwex=boxwex, ylim = ylims[[1]],yaxs="i",las=2,...)
# mtext(side=2,expression(paste(B[2011]," (",10^3,"mt)")),outer=F,line=2.7, cex=0.9)

 Bdepletion <- list()
 for (i in 1:nfiles)  {
   Bcurrent.column <- grep("B.2011",colnames(BdataList[[i]])) 
   Binit.column <- grep("B.1931",colnames(BdataList[[i]])) 
   Bdepletion[[i]] <- (BdataList[[i]])[,Bcurrent.column] / (BdataList[[i]])[,Binit.column]
 }
 boxplot(Bdepletion,names=rep("",nfiles),boxwex=boxwex, ylim = ylims[[1]],yaxs="i",las=2,...)
 mtext(side=2,expression(B[2011]/B[0]),outer=F,line=2.7, cex=0.9)


 Bdepletion <- list()
 for (i in 1:nfiles)  {
   Bcurrent.column <- grep("B.2020",colnames(BdataList[[i]])) 
   Binit.column <- grep("B.1931",colnames(BdataList[[i]])) 
   Bdepletion[[i]] <- (BdataList[[i]])[,Bcurrent.column] / (BdataList[[i]])[,Binit.column]
 }
 boxplot(Bdepletion,names=rep("",nfiles),boxwex=boxwex, ylim = ylims[[2]],yaxs="i",las=2,...)
 mtext(side=2,expression(B[2020]/B[0]),outer=F,line=2.7, cex=0.9)
 
 Bdepletion <- list()
 for (i in 1:nfiles)  {
   Bcurrent.column <- grep("B.2014",colnames(BdataList[[i]])) 
   Binit.column <- grep("B.2004",colnames(BdataList[[i]])) 
   Bdepletion[[i]] <- (BdataList[[i]])[,Bcurrent.column] / (BdataList[[i]])[,Binit.column]
 }
 boxplot(Bdepletion,names=scenario.descr,boxwex=boxwex, ylim = ylims[[3]],yaxs="i",las=2,...)
 mtext(side=2,expression(B[2014]/B[2004]),outer=F,line=2.7, cex=0.9)

 Bdepletion <- list()
 for (i in 1:nfiles)  {
   Bcurrent.column <- grep("B.2025",colnames(BdataList[[i]])) 
   Binit.column <- grep("B.2011",colnames(BdataList[[i]])) 
   Bdepletion[[i]] <- (BdataList[[i]])[,Bcurrent.column] / (BdataList[[i]])[,Binit.column]
 }
 boxplot(Bdepletion,names=scenario.descr,boxwex=boxwex, ylim = ylims[[4]],yaxs="i",las=2,...)
 mtext(side=2,expression(B[2025]/B[2011]),outer=F,line=2.7, cex=0.9)


 mtext(side = 1, outer = T, xlab,line=3.5)
 mtext(side = 3, outer = T, title)
 
 invisible(BdataList)
}

