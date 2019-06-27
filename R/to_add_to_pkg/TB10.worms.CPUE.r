#' Worm CPUE f
#'
#' Plots n.worms (realizations), the median and the 10-90th percentiles of biomass
#'
#' @return a plot
#' @examples
#' # Fig. 15 Worm plots for CPUE
#' worm.CPUE.f(s4filename = "CURRENT/v0/CURRENT_c1s1l13h.s4", title = "Scenario c1s1l13h, current catches")
#' @export
#' 
worm.CPUE.f <- function(s4filename, n.worms = 10, ylim = c(0,1), xlim = c(2005,2040), seed = 1, title = "")
{
   set.seed(seed)
   .Options$warn <- -1  # suppress annoying warning messages
   tmp <- read.table(s4filename,skip=2,header=T)

   #find data years
   min.yr <- floor(xlim[2])
   max.yr <- ceiling(xlim[1])
   for (yr in ceiling(xlim[1]):floor(xlim[2])) {
      if(length(grep(paste("CPUE.",yr,sep=""),colnames(tmp)))>=1) {
         if (yr<min.yr) {min.yr <- yr}
         if (yr>max.yr) {max.yr <- yr}
      }
   }
   data.columns <- (grep(paste("CPUE.",min.yr,sep=""),colnames(tmp))):(grep(paste("CPUE.",max.yr,sep=""),colnames(tmp)) )
   yr.first <- min.yr
   yr.last <- max.yr
   worms <- tmp[,data.columns]

   # Using the quantile function does not give the exact same anwer as Vivian
   # gets in her summary files; instead calculate quantiles the following way:
   n <- nrow(worms)
   quantiles<- apply(worms,2,function(x){sort(x)[c(max(1,trunc(.1*n)),ceiling(.5*n),min(n,ceiling(.9*n)+1))]})

   par(mfrow=c(1,1),mai=c(0.3,1,0,.2),omi=c(0.5,0,0.6,0))
   par(cex.axis=.8)

   #find some worms
   K <- min(nrow(worms),n.worms)
   worms.index <- sample(1:nrow(worms),K)

   if(missing(ylim)) ylim <- c(0,max(worms[worms.index,],quantiles))
   else ylim <- ylim
   plot(yr.first:yr.last,quantiles[2,],ylim=ylim,
     xlab="",ylab="",  xlim=c(yr.first-.5,yr.last+.5),xaxt="n",cex=1,yaxs="i",xaxs="i",las=1)
   polygon(c(yr.first:yr.last,yr.last:yr.first),c(quantiles[1,],
     rev(quantiles[3,])),col=13,border=NA) 
   lines(yr.first:yr.last,quantiles[2,],type="o",lwd=2,col="red")
   if(n.worms>0) { 
     for(i in worms.index)
       lines(yr.first:yr.last,worms[i,])
   }
   axis(side=1,cex=1)
   mtext("Year",side=1,outer=F,cex=1.3,line=2.5)
   mtext(side=2,outer=F,cex=1.3,line=3,"CPUE")
   mtext(title,side=3,outer=T,line=1.3,cex=1)
}
