###############################################
#Plots the fits to the tagging data for sbtmod22.tpl
#outputs from a _lab.rep file, assuming that the naming convention in the file has sections of 
#outputs separated by $name comments so that it can be easily parsed by readList() function of Jon Schnute 
#IMPORTANT: library PBSmodelling required, available at http://code.google.com/p/pbs-software/downloads/list
#then in R go to menu Packages, Install Package from Local zip file
####RUN this by typing , expects subdirectory \figs
#source("Tagging fits combine tagger groups.r")
###############################################
tagging.fits <- function(data.object,case_label="c1s1l1orig.5_h1m1M1O1C2a1") {
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
#pdf("figs\\Tagfits combine taggers.pdf",width=6,height=8)
#tagging.fits(data.object = x[[1]],case_label="example")
#dev.off()
