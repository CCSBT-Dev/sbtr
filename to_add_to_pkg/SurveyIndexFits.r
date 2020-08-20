##########################################################################
#Plots fits of the CCSBT model sbtmod22.tpl to the survey and index data:
#Aerial survey, troll survey, commercial spotter index, and CPUE index.
#Produces a four-panel plot with data and model fits to the data. 
#Programmed by Trevor A. Branch 25 June 2009
####source("SurveyIndexFits.r")
##########################################################################
fit.CPUE.index <- function(data.object,lab.cex=lab.cex) {
   x <- data.object
   years <- x$yrs.cpue[1]:x$yrs.cpue[2]
   cpue.obs <- x$cpue
   cpue.pred <- x$cpue.pred

   ylim <- c(0,1.1*max(c(cpue.obs,cpue.pred)))
   plot(years, cpue.obs, type="p",cex=1.3,pch=19,col="black", ylim=ylim, las=1,yaxs="i",xlab="",ylab="")
   par(new=T)
   plot(years, cpue.pred, type="l",lwd=2,col="blue",ylim=ylim, las=1,yaxs="i",xlab="",ylab="")

   #mtext(side=1,line=2.8,"Year",cex=lab.cex)
   mtext(side=2,line=3,"CPUE index",cex=lab.cex)
}
#fit.CPUE.index(labrep.file="sbtmod22_lab.rep")

fit.aerial.survey<- function(data.object,lab.cex=1.1) {
   x <- data.object
   years <- x$Aerial.Surv[,1]
   cpue.obs <- x$Aerial.Surv[,2]
   cpue.pred <- x$Aerial.Surv[,3]
   #cpue.pred[cpue.obs < 0] <- NA     #missing years are indicated in the data by -999
   cpue.obs[cpue.obs < 0] <- NA     #missing years are indicated in the data by -999

   ylim <- c(0,1.1*max(c(cpue.obs,cpue.pred),na.rm=T))
   plot(years, cpue.obs, type="p",cex=1.3,pch=19,col="black", ylim=ylim, las=1,yaxs="i",xlab="",ylab="")
   par(new=T)
   plot(years, cpue.pred, type="l",lwd=2,col="blue",ylim=ylim, las=1,yaxs="i",xlab="",ylab="")

   #mtext(side=1,line=2.8,"Year",cex=lab.cex)
   mtext(side=2,line=3,"Aerial survey index",cex=lab.cex)
}
#fit.aerial.survey(labrep.file="sbtmod22_lab.rep")


fit.troll.survey<- function(data.object,lab.cex=1.1) {
   x <- data.object
   years <- x$Troll.Index[,1]
   cpue.obs1 <- x$Troll.Index[,2]
   cpue.obs2 <- x$Troll.Index[,3]
   cpue.obs3 <- x$Troll.Index[,4]
   cpue.pred <- x$Troll.Index[,5]

   cpue.obs1[cpue.obs1 < 0] <- NA     #missing years are indicated in the data by -999
   cpue.obs2[cpue.obs2 < 0] <- NA     #missing years are indicated in the data by -999
   cpue.obs3[cpue.obs3 < 0] <- NA     #missing years are indicated in the data by -999
   cpue.pred[is.na(cpue.obs1) & is.na(cpue.obs2) & is.na(cpue.obs3)] <- NA
   
   ylim <- c(0,1.1*max(c(cpue.obs1,cpue.obs2,cpue.obs3,cpue.pred),na.rm=T))
   par(xpd=T)  # so that the circles exactly on the x-axis are not obscured
   plot(years, cpue.obs1, type="p",cex=1.3,pch=19,col="black", ylim=ylim, las=1,yaxs="i",xlab="",ylab="")
   par(new=T)
   plot(years, cpue.obs2, type="p",cex=1.3,pch=19,col="grey50", ylim=ylim, las=1,yaxs="i",xlab="",ylab="")
   par(new=T)
   plot(years, cpue.obs3, type="p",cex=1.3,pch=19,col="red", ylim=ylim, las=1,yaxs="i",xlab="",ylab="")
   par(new=T)
   plot(years, cpue.pred, type="l",lwd=2,col="blue",ylim=ylim, las=1,yaxs="i",xlab="",ylab="")
   par(xpd=F)

   mtext(side=1,line=2.8,"Year",cex=lab.cex)
   mtext(side=2,line=3,"Troll index",cex=lab.cex)
}
#fit.troll.survey(labrep.file="sbtmod22_lab.rep",lab.cex=1.1)

fit.all <- function(data.object,lab.cex=1.1,case_label="c1s1l1orig.5_h1m1M1O1C2a1")  {
   par(mfrow=c(3,1),oma=c(3,2,1,0),mar=c(2,3,1,1))
   fit.CPUE.index(data.object=data.object,lab.cex=lab.cex)
   fit.aerial.survey(data.object=data.object,lab.cex=lab.cex)
   fit.troll.survey(data.object=data.object,lab.cex=lab.cex)
   mtext(side=3,line=-0.2,outer=T,case_label,cex=0.6)
}
#pdf(file="figs\\SurveyIndexFits.pdf",width=4,height=8)
#win.graph(width=6,height=11.5) 
#fit.all(data.object=x[[1]],lab.cex=1.1,case_label="example")
#dev.off()
