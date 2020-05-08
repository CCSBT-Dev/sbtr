#' Worm SSB f
#' 
#' Plots n.worms (realizations), the median and the 10-90th percentiles of biomass
#' 
#' @export
#' 
worm.SSB.f <- function(s3filename,n.worms=10,ylim=c(0,20), xlim=c(2005,2040), seed=1, title="")
{
   set.seed(seed)
   .Options$warn <- -1  # suppress annoying warning messages
   tmp <- read.table(s3filename,skip=2,header=T)

   #find data years
   min.yr <- floor(xlim[2])
   max.yr <- ceiling(xlim[1])
   for (yr in ceiling(xlim[1]):floor(xlim[2])) {
      if(length(grep(paste("B.",yr,sep=""),colnames(tmp)))>=1) {
         if (yr<min.yr) {min.yr <- yr}
         if (yr>max.yr) {max.yr <- yr}
      }
   }
   data.columns <- (grep(paste("B.",min.yr,sep=""),colnames(tmp))):(grep(paste("B.",max.yr,sep=""),colnames(tmp)) )
   yr.first <- min.yr
   yr.last <- max.yr
   bworms <- tmp[,data.columns] /1000000

   # Using the quantile function does not give the exact same anwer as Vivian
   # gets in her summary files; instead calculate quantiles the following way:
   n <- nrow(bworms)
   B.quantiles<- apply(bworms,2,function(x){sort(x)[c(max(1,trunc(.1*n)),ceiling(.5*n),min(n,ceiling(.9*n)+1))]})

   par(mfrow=c(1,1),mai=c(0.3,1,0,.2),omi=c(0.5,0,0.6,0))
   par(cex.axis=.8)

   #find some worms
   K <- min(nrow(bworms),n.worms)
   worms.index <- sample(1:nrow(bworms),K)

   if(missing(ylim)) ylim <- c(0,max(bworms[worms.index,],B.quantiles))
   else ylim <- ylim
   plot(yr.first:yr.last,B.quantiles[2,],ylim=ylim,
     xlab="",ylab="",  xlim=c(yr.first-.5,yr.last+.5),xaxt="n",cex=1,yaxs="i",xaxs="i",las=1)
   polygon(c(yr.first:yr.last,yr.last:yr.first),c(B.quantiles[1,],
     rev(B.quantiles[3,])),col=13,border=NA) 
   lines(yr.first:yr.last,B.quantiles[2,],type="o",lwd=2,col="red")
   if(n.worms>0) { 
     for(i in worms.index)
       lines(yr.first:yr.last,bworms[i,])
   }
   axis(side=1,cex=1)
   mtext("Year",side=1,outer=F,cex=1.3,line=2.5)
   mtext(side=2,outer=F,cex=1.3,line=3,expression(paste("Spawning biomass ( ",10^6," mt)",sep="")))
   mtext(title,side=3,outer=T,line=1.3,cex=1)
}
