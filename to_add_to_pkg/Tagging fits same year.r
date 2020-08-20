###############################################
#Plots the fits to the pooled tagging data for sbtmod22.tpl
#outputs from a _lab.rep file, assuming that the naming convention in the file has sections of 
#outputs separated by $name comments so that it can be easily parsed by readList() function of Jon Schnute 
#IMPORTANT: library PBSmodelling required, available at http://code.google.com/p/pbs-software/downloads/list
#then in R go to menu Packages, Install Package from Local zip file
###############################################
tagging.fits.same.age <- function(data.object, ages=1:8, years=1991:1997,case_label="c1s1l1orig.5_h1m1M1O1C2a1") {
   x <- data.object

   tagobs <- x$pooled.1yeartag.obs
   tagpred <- x$pooled.1yeartag.pred

   nyears <- nrow(tagobs)
   nages <- ncol(tagobs)

   mfrow <- c(nyears,1)
   par(mfrow=mfrow,oma=c(5,5,4.5,1),mar=c(0,0,0,0))

   for (yr in 1:nyears) { 
      ylim <- c(0,1.1*max(tagobs[yr,],tagpred[yr,],1))
      names.arg <- rep("",nages)
      total.recaps <- sum(tagobs[yr,])
      x <- barplot(tagobs[yr,],space=0.2,ylim=ylim,las=1,names.arg=names.arg, cex.names=0.5, xaxs="i",yaxs="i",col="grey70",
                        axes=F,ylab="",xlab="")
      if (yr >= nyears) {
         axis(side=1,at=x,lab=ages, lwd=0,lwd.ticks=0)  #just use for the labels, to allow more control than names.arg
      }
      y.labs <- pretty(ylim)
      if (yr==1) {
         axis(2,las=1,at=y.labs)
      }
      else {
         nlabs <- length(y.labs)
         y.labs <- y.labs[-c(nlabs-1,nlabs)]
         axis(2,las=1,at=y.labs)
      }
      par(new=T)
      par(xpd=NA)
      plot(x=x,y=tagpred[yr,],ylim=ylim, xlim=par("usr")[1:2], las=1,xaxs="i",yaxs="i",col="black",pch=19,cex=1.5,axes=F,ylab="",xlab="")
      box(col="black",lwd=1)
      x.pos <- par("usr")[1] + 0.85*diff(par("usr")[1:2])   #par("usr") spits out the current coordinates of the plot window
      y.pos <- par("usr")[3] + 0.8*diff(par("usr")[3:4])   #par("usr") spits out the current coordinates of the plot window
      text(x=x.pos,y=y.pos,years[yr],cex=1.3)
   }
   mtext(side=1,outer=T,"Recapture ages",line=3)
   mtext(side=2,outer=T,"Number of recaptures",line=3.5)
   mtext(side=3,outer=T,line=2.3,"Recaptures in year of tagging")
   mtext(side=3,outer=T,line=0.8,cex=0.5,paste("(",case_label,")",sep=""))

}
#pdf("figs\\Tagfits same year.pdf",width=3,height=8)
#win.graph(width=5,height=11.5)
#Does not work on sbtmod21 (since the tagging likelihood there is on pooled taggging data)
#tagging.fits.same.age(data.object = x[[1]], ages=1:8, years=1991:1997,case_label="c1s1l1orig.5_h1m1M1O1C2a1")
#dev.off()
