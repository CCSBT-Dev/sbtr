#' Plots the fits to the tagging data
#' 
#' Plots the fits to the tagging data for sbtmod22.tpl outputs from a _lab.rep file, assuming that the naming convention in the file has sections of outputs separated by $name comments so that it can be easily parsed by readList() function of Jon Schnute. RUN this by typing , expects subdirectory figs/
#'
#' @examples
#' pdf("figs/Tagfits combine taggers.pdf", width = 6, height = 8)
#' plot_tagging_fits(data.object = x[[1]], case_label = "example")
#' dev.off()
#' @export
#' 
plot_tagging_fits <- function(data.object, case_label = "c1")
{
   year.list <- 1989:1994
   nyears <- length(year.list)
   age.list <- 1:3
   nages <- length(age.list)
   ntaggers <- 6

   x <- data.object
   tagobs <- x$tag.obs
   tagpred <- x$tag.pred
   tagres <- x$tag.res

   row.labels <- matrix(nrow=nrow(tagobs),ncol=3,dimnames=list(1:nrow(tagobs),c("RelAge","TaggerGroup","CohortYear")))
   tg <- 1
   cy <- 1989
   for (i in 1:nrow(tagobs)) {
      row.labels[i,"RelAge"] <- (i-1)%%3 + 1
      row.labels[i,"TaggerGroup"] <- tg
      row.labels[i,"CohortYear"] <- cy
      if (i%%3==0) { 
         tg<-tg+1 
         if(tg==7) { tg<-1 }
      }
      if (i%%18==0) { cy<-cy+1 }
   }
   #print(row.labels)

   nobs <- nrow(tagobs)
   ncols <- ncol(tagobs)
   
   mfrow <- c(nyears,nages)
   nrows <- prod(mfrow)
   pooled.tagobs <- matrix(nrow=nrows,ncol=2+7,dimnames=list(1:18,c("RelAge","CohortYear",paste("Age",1:7))))
   pooled.tagpred <- matrix(nrow=nrows,ncol=2+7,dimnames=list(1:18,c("RelAge","CohortYear",paste("Age",1:7))))
   
   cy <- 1989
   for (i in 1:nrows) {
       relage <- (i-1)%%3 + 1
       pooled.tagobs[i,1] <- relage 
       pooled.tagpred[i,1] <- relage
       pooled.tagobs[i,2] <- cy
       pooled.tagpred[i,2] <- cy
       pooled.tagobs[i,3:9] <- colSums(tagobs[row.labels[,"CohortYear"]==cy  & row.labels[,"RelAge"]==relage,])    #sums up over the tagger groups
       pooled.tagpred[i,3:9] <- colSums(tagpred[row.labels[,"CohortYear"]==cy  & row.labels[,"RelAge"]==relage,])    #sums up over the tagger groups
       if (i%%3==0) { cy<-cy+1 }
   }
   #print(pooled.tagobs)
   #print(pooled.tagpred)
   
   par(mfrow=mfrow,oma=c(3.5,2.5,5.5,4.5),mar=c(0,0,0,0))

   for (yr in 1:nyears) {
      for (a in 1:nages) {
         obsdata <- pooled.tagobs[pooled.tagobs[,1]==age.list[a] & pooled.tagobs[,2]==year.list[yr],3:9]      
         preddata <- pooled.tagpred[pooled.tagobs[,1]==age.list[a] & pooled.tagobs[,2]==year.list[yr],3:9]      
         ylim <- c(0,1.2*max(obsdata,preddata,1))
         names.arg <- rep("",ncols)
         x <- barplot(obsdata,space=0.3,ylim=ylim,las=1,names.arg=names.arg, cex.names=0.5, xaxs="i",yaxs="i",col="gray70",
                           axes=F,ylab="",xlab="")
         if (yr==nyears) {
           axis(side=1,at=x[seq(1,7,2)],line=-0.8,c(1:7)[seq(1,7,2)],lwd=0,lwd.ticks=0,cex.axis=1)   #suppress axis lines and ticks, just put in labels
           axis(side=1,at=x[seq(2,6,2)],line=-0.8,c(1:7)[seq(2,6,2)],lwd=0,lwd.ticks=0,cex.axis=1)   #suppress axis lines and ticks, just put in labels
         }      
         par(new=T)
         par(xpd=NA)
         if (yr >= 3) {
           from.val <- yr-ncols-3
           exclude <- from.val:(-1*ncols)
         }
         else {
           exclude <- 1:ncols
         }
         plot(x=x[exclude],y=preddata[exclude],ylim=ylim, xlim=par("usr")[1:2], las=1,xaxs="i",yaxs="i",col="black",pch=19,cex=1.4,axes=F,ylab="",xlab="")
         box(col="gray50",lwd=0.3)
         par(xpd=T)
         if (yr==1) {
            mtext(side=3,outer=F,line=0.7,paste("Rel age",age.list[a]),cex=1)
         }
         if (a==nages) {
            mtext(side=4,year.list[yr],outer=F,line=1,cex=1)
         }
         x.pos <- par("usr")[1] + 0.85*diff(par("usr")[1:2])   #par("usr") spits out the current coordinates of the plot window
         y.pos <- par("usr")[3] + 0.85*diff(par("usr")[3:4])   #par("usr") spits out the current coordinates of the plot window
         text(x=x.pos,y=y.pos,paste("n =",sum(obsdata)),cex=1.1)

      }
   }
   mtext(side=1,outer=T,line=2,"Recapture ages",cex=1)
   mtext(side=2,outer=T,line=1,"Number of recaptures",cex=1)
   mtext(side=3,outer=T,line=3,case_label,cex=1)
   invisible(row.labels)
}


#' Plots side by side fits to tagging data from two lab_rep files.
#' 
#' Assumes that the _lab.rep files have been read in using code like x <- readList(file="example_lab.rep") and that compare.objects is a list. 
#'
#' @export
#' 
plot_tagging_fits_pooled_compare <- function(compare.objects, case.label, ages=2:8, years=1992:1997)
{
   library(PBSmodelling)
   x <- compare.objects[[1]]   #yes, inefficient.
   tagobs <- x$pooled.tag.obs
   tagpred <- x$pooled.tag.pred
   nyears <- nrow(tagobs)
   nages <- ncol(tagobs)
   mfrow <- c(nyears,2)
   par(mfcol=mfrow,oma=c(5,5,4.5,1),mar=c(0,0,0,0))

   for (i in 1:2)
   {
      x <- compare.objects[[i]]
      tagobs <- x$pooled.tag.obs
      tagpred <- x$pooled.tag.pred
      for (yr in 1:nyears) { 
         ylim <- c(0,1.1*max(tagobs[yr,],tagpred[yr,],1))
         names.arg <- rep("",nages)
         total.recaps <- sum(tagobs[yr,])
         x <- barplot(tagobs[yr,],space=0.2,ylim=ylim,las=1,names.arg=names.arg, cex.names=0.5, xaxs="i",yaxs="i",col="grey70",
                           axes=F,ylab="",xlab="")
         if (yr >= nyears)
         {
            axis(side=1,at=x,lab=ages, lwd=0,lwd.ticks=0)  #just use for the labels, to allow more control than names.arg
         }
         y.labs <- pretty(ylim)
         if (yr==1  && i==1)
         {
            axis(2,las=1,at=y.labs)
         }
         else 
           if (i==1) {
            nlabs <- length(y.labs)
            y.labs <- y.labs[-c(nlabs-1,nlabs)]
            axis(2,las=1,at=y.labs)
         }
         par(new=T)
         par(xpd=NA)
         plot(x=x,y=tagpred[yr,],ylim=ylim, xlim=par("usr")[1:2], las=1,xaxs="i",yaxs="i",col="black",pch=19,cex=1.5,axes=F,ylab="",xlab="")
         par(xpd=T)
         box(col="black",lwd=1)
         x.pos <- par("usr")[1] + 0.85*diff(par("usr")[1:2])   #par("usr") spits out the current coordinates of the plot window
         y.pos <- par("usr")[3] + 0.8*diff(par("usr")[3:4])   #par("usr") spits out the current coordinates of the plot window
         if (i==2)
         {
           text(x=x.pos,y=y.pos,years[yr],cex=1.3)
         }
      }
   }
   mtext(side=1,outer=T,"Recapture ages",line=3)
   mtext(side=2,outer=T,"Number of recaptures",line=3.5)
   mtext(side=3,outer=T,line=2.3,"Recaptures  in subsequent years")
   mtext(side=3,outer=T,line=0.8,cex=0.7,paste("(",case.label,")",sep=""))

}


#' Plots the fits to the pooled tagging data.
#' 
#' Plots the fits to the pooled tagging data for sbtmod22.tpl outputs from a _lab.rep file.
#'
#' @examples
#' pdf("figs/Tagfits pooled.pdf", width = 3, height = 8)
#' # Does not work on sbtmod21 (since the tagging likelihood there is on pooled taggging data)
#' tagging.fits.pooled(data.object = x[[1]], ages = 2:8, years = 1992:1997, case_label="c1")
#' dev.off()
#' @export
#' 
plot_tagging_fits_pooled <- function(data.object, ages=2:8, years=1992:1997,case_label="c1s1l1orig.5_h1m1M1O1C2a1")
{
   x <- data.object

   tagobs <- x$pooled.tag.obs
   tagpred <- x$pooled.tag.pred

   nyears <- nrow(tagobs)
   nages <- ncol(tagobs)

   mfrow <- c(nyears,1)
   par(mfrow=mfrow,oma=c(5,5,4.5,1),mar=c(0,0,0,0))

   for (yr in 1:nyears)
   { 
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
   mtext(side=3,outer=T,line=2.3,"Recaptures  in subsequent years")
   mtext(side=3,outer=T,line=0.8,cex=0.7,paste("(",case_label,")",sep=""))

}


#' @title Plots the fits to the tagging data
#' @description
#' Plots the fits to the pooled tagging data for sbtmod22.tpl outputs from a _lab.rep file, assuming that the naming convention in the file has sections of outputs separated by $name comments so that it can be easily parsed by readList() function of Jon Schnute 
#' @export
#' 
plot_tagging_fits_same_age <- function(data.object, ages=1:8, years=1991:1997,case_label="c1s1l1orig.5_h1m1M1O1C2a1")
{
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


#' Plots the fits to the tagging data for sbtmod22.tpl
#'
#' outputs from a _lab.rep file, assuming that the naming convention in the file has sections of 
#' outputs separated by $name comments so that it can be easily parsed by readList() function of Jon Schnute 
#' RUN this by typing source("Tagging fits subsequent.r"), expects subdirectory figs
#' @export
#'
plot_tagging_fits_subsequent <- function(data.object,case_label="c1s1l1orig.5_h1m1M1O1C2a1") {
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
