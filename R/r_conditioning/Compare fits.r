##########################################################################
#Plots fits of the CCSBT model sbtmod22.tpl to the CPUE and aerial surveys;
#troll survey excluded.
#Programmed by Trevor A. Branch 25 June 2009
##########################################################################
fit.CPUE.index <- function(compare.objects,lab.cex=1.1) {
   nfiles <- length(compare.objects)
   cpue.pred <- NULL
   for (i in 1:nfiles) {
      x <- compare.objects[[i]]
      cpue.pred <- rbind(cpue.pred,x$cpue.pred)
   }
   years <- x$yrs.cpue[1]:x$yrs.cpue[2]
   cpue.obs <- x$cpue

   #ylim <- c(0,1.1*max(c(cpue.obs,cpue.pred)))
   ylim <- c(0,3.2)
   plot(years, cpue.obs, type="p",cex=1.3,pch=19,col="black", ylim=ylim, las=1,yaxs="i",xlab="",ylab="")
   for (i in 1:nfiles) {
     par(new=T)
     plot(years, cpue.pred[i,], type="l",lwd=2,col=i+1,ylim=ylim, las=1,yaxs="i",xlab="",ylab="")
   }
   
   #mtext(side=1,line=2.8,"Year",cex=lab.cex)
   mtext(side=2,line=3,"CPUE index",cex=lab.cex)
}
#fit.CPUE.index(labrep.file="sbtmod22_lab.rep")

fit.aerial.survey<- function(compare.objects,lab.cex=1.1) {
   library(PBSmodelling)
   nfiles <- length(compare.objects)
   
   cpue.pred <- NULL
   for (i in 1:nfiles) {
      x <- compare.objects[[i]]
      cpue.pred <- rbind(cpue.pred,t(x$Aerial.Surv[,3]))
   }
  
   
   cpue.obs <- x$Aerial.Surv[,2]
   #cpue.pred[,cpue.obs<0] <- NA
   
   years <- x$Aerial.Surv[,1]
   cpue.obs[cpue.obs < 0] <- NA     #missing years are indicated in the data by -999

   ylim <- c(0,1.1*max(c(cpue.obs,cpue.pred),na.rm=T))
   plot(years, cpue.obs, type="p",cex=1.3,pch=19,col="black", ylim=ylim, las=1,yaxs="i",xlab="",ylab="")
   
   for (i in 1:nfiles) {
     par(new=T)
     plot(years, cpue.pred[i,], type="l",lwd=2,col=i+1,ylim=ylim, las=1,yaxs="i",xlab="",ylab="")
   }
   
   #mtext(side=1,line=2.8,"Year",cex=lab.cex)
   mtext(side=2,line=3,"Aerial survey index",cex=lab.cex)
}
#fit.aerial.survey(labrep.file="sbtmod22_lab.rep")

fit.troll.survey<- function(labrep.files,lab.cex=1.1) {
   library(PBSmodelling)
   nfiles <- length(labrep.files)
   cpue.pred <- NULL
   for (i in 1:nfiles) {
      x <- readList(labrep.files[i])
      cpue.pred <- rbind(cpue.pred,t(x$Troll.Index[,5]))
   }
   years <- x$Troll.Index[,1]
   cpue.obs1 <- x$Troll.Index[,2]
   cpue.obs2 <- x$Troll.Index[,3]
   cpue.obs3 <- x$Troll.Index[,4]

   cpue.obs1[cpue.obs1 < 0] <- NA     #missing years are indicated in the data by -999
   cpue.obs2[cpue.obs2 < 0] <- NA     #missing years are indicated in the data by -999
   cpue.obs3[cpue.obs3 < 0] <- NA     #missing years are indicated in the data by -999
   cpue.pred[,is.na(cpue.obs1) & is.na(cpue.obs2) & is.na(cpue.obs3)] <- NA
   
   ylim <- c(0,1.1*max(c(cpue.obs1,cpue.obs2,cpue.obs3,cpue.pred),na.rm=T))
   par(xpd=T)  # so that the circles exactly on the x-axis are not obscured
   plot(years, cpue.obs1, type="p",cex=1.3,pch=19,col="black", ylim=ylim, las=1,yaxs="i",xlab="",ylab="")
   par(new=T)
   plot(years, cpue.obs2, type="p",cex=1.3,pch=19,col="grey50", ylim=ylim, las=1,yaxs="i",xlab="",ylab="")
   par(new=T)
   plot(years, cpue.obs3, type="p",cex=1.3,pch=19,col="red", ylim=ylim, las=1,yaxs="i",xlab="",ylab="")
   for (i in 1:nfiles) {
     par(new=T)
     plot(years, cpue.pred[i,], type="l",lwd=2,col=i+1,ylim=ylim, las=1,yaxs="i",xlab="",ylab="")
   }
   par(xpd=F)

   mtext(side=1,line=2.8,"Year",cex=lab.cex)
   mtext(side=2,line=3,"Troll index",cex=lab.cex)
}
#fit.troll.survey(labrep.file="sbtmod22_lab.rep",lab.cex=1.1)

compare.fits <- function(compare.objects,lab.cex=1.1,case_label,legend)  {
   par(mfcol=c(2,1),oma=c(1,3,1,0),mar=c(2,2,1,1))
   fit.CPUE.index(compare.objects=compare.objects,lab.cex=lab.cex)
   fit.aerial.survey(compare.objects=compare.objects,lab.cex=lab.cex)
   legend(x=2002,y=2,legend=legend,lty=1,lwd=2,col=1:length(compare.objects)+1,bty="n")
   #fit.troll.survey(compare.objects=compare.objects,lab.cex=lab.cex)
   mtext(side=3,line=-0.2,outer=T,case_label,cex=1)
}
#pdf(file="figs\\SurveyIndexFits.pdf",width=4,height=8)
#win.graph(width=6,height=11.5) 
#compare.fits(compare.objects=objs[1:2],lab.cex=1.1,case_label="example", legend=names(objs[1:2]))
#dev.off()
