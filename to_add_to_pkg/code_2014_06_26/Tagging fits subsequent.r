###############################################
#Plots the fits to the tagging data for sbtmod22.tpl
#outputs from a _lab.rep file, assuming that the naming convention in the file has sections of 
#outputs separated by $name comments so that it can be easily parsed by readList() function of Jon Schnute 
####RUN this by typing source("Tagging fits subsequent.r"), expects subdirectory \figs
###############################################
tagging.fits <- function(data.object,case_label="c1s1l1orig.5_h1m1M1O1C2a1") {
   x <- data.object
   tagobs <- x$tag.obs
   tagpred <- x$tag.pred
   tagres <- x$tag.res

   nobs <- nrow(tagobs)
   ncols <- ncol(tagobs)

   mfrow <- c(6,18)
   par(mfrow=mfrow,oma=c(3.5,2.5,3.5,4.5),mar=c(0,0,0,0))

   figure.row <- 1
   year <- 1989
   tagger <- 1
   rel.age <- 1
   for (i in 1:nobs) {
      ylim <- c(0,1.1*max(tagobs[i,],tagpred[i,],1))
      names.arg <- rep("",ncols)
      total.recaps <- sum(tagobs[i,])
      if (total.recaps <5) { p.color <- heat.colors(7)[7] }
      if (total.recaps >= 5 & total.recaps <20) { p.color <- heat.colors(7)[5] }
      if (total.recaps >= 20 & total.recaps <100) { p.color <- heat.colors(7)[3] }
      if (total.recaps >= 100) { p.color <- heat.colors(7)[1] }
      x <- barplot(tagobs[i,],space=0.3,ylim=ylim,las=1,names.arg=names.arg, cex.names=0.5, xaxs="i",yaxs="i",col=p.color,
                        axes=F,ylab="",xlab="")
      if (i > (mfrow[1]-1)*mfrow[2]) {
        axis(side=1,at=x[seq(1,7,2)],line=-0.8,c(1:7)[seq(1,7,2)],lwd=0,lwd.ticks=0,cex.axis=0.8)   #suppress axis lines and ticks, just put in labels
        axis(side=1,at=x[seq(2,6,2)],line=-0.8,c(1:7)[seq(2,6,2)],lwd=0,lwd.ticks=0,cex.axis=0.8)   #suppress axis lines and ticks, just put in labels
      }      
      par(new=T)
      par(xpd=NA)
      if (figure.row >= 3) {
        from.val <- figure.row-ncols-3
        exclude <- from.val:(-1*ncols)
      }
      else {
        exclude <- 1:ncols
      }
      plot(x=x[exclude],y=tagpred[i,exclude],ylim=ylim, xlim=par("usr")[1:2], las=1,xaxs="i",yaxs="i",col="black",pch=19,cex=0.8,axes=F,ylab="",xlab="")
      box(col="gray50",lwd=0.3)

      par(xpd=T)
      if (figure.row==1) {
         if((i-2)%%3 == 0)  {   #above the 2, 5, 8, etc figures
            mtext(side=3,outer=F,line=2,paste("Tagger group",tagger))
            tagger <- tagger+1
         }
         mtext(side=3,outer=F,line=0.7,paste("Rel age",rel.age),cex=0.7)
         rel.age <- rel.age+1
         if(rel.age > 3) {rel.age <- 1}
      }
      if (i%%mfrow[2]==0) {
         figure.row <- figure.row+1
         mtext(side=4,year,outer=F,line=1)
         year <- year+1
      }
   }
   mtext(side=1,outer=T,line=2,paste("Recapture ages","(", case_label,")"))
   mtext(side=2,outer=T,line=1,"Number of recaptures")
   mtext(side=4,outer=T,line=3,"Cohort year")
}
#pdf("figs\\Tagfits subsequent.pdf",width=12,height=5)
#win.graph(width=12,height=5)
#Does not work on sbtmod21 (since the tagging likelihood there is on pooled taggging data)
#tagging.fits(labrep.file = "sbtmod22_lab.rep",case_label="c1s1l1orig.5_h1m1M1O1C2a1")
#dev.off()
