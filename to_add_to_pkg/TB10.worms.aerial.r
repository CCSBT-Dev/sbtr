#' Worm aerial plot
#' 
#' Worm plot examining the range and 10 worms for Australian aerial surveys, in the projections. Each worm is one realization from the projections (out of 2000), while the mean is shown in red with circles, and the shaded area is the 90th (95th?) percentile range.
#' 
#' @param s7filename full path from R directory include full filename of .s7 file
#' @param n.worms number of black lines to plot
#' @param ylim min and max of y-axis to plot
#' @param seed integer specifying the seed for the random number generator (thus allowing for the same realizations to be compared between plots) (default=1)
#' @return a plot
#' @examples
#' pdf(file="figs/aerial.pdf", height = 9, width = 8)
#' for (i in 1:10)
#' {
#'   x <- worms.aerial(s7filename = "CURRENT_c1s1l13h.s7", withError = 1, n.worms = 8, title = "Continue with 2011 catches", seed = i, ylim = c(0,7))
#' }
#' dev.off()
#' @export
#' 
worms.aerial <- function(s7filename = "CURRENT_c1s1l13h.s7", withError = 1, n.worms = 10, ylim, seed = 1, title = NULL, yr.first = 2009) 
{
   set.seed(seed)
   .Options$warn <- -1  # suppress annoying warning messages
   tmp <- read.csv(file=s7filename,sep=" ")
   if(withError == 1)
   { 
      aer.cols <- sort(grep(pattern="AerWithErr",x=colnames(tmp)))
   }
   else
   {
      aer.cols <- sort(grep(pattern="AerNoErr",x=colnames(tmp)))
   }
   tmp <- as.matrix(tmp[,aer.cols])
   nyr <- ncol(tmp)
   yr.last <- yr.first+nyr-1
   

   # Using the quantile function does not give the exact same anwer as Vivian
   # gets in her summary files; instead calculate quantiles the following way:
   n <- nrow(tmp)
   quantiles<- apply(tmp,2,function(x){sort(x)[c(max(1,trunc(.1*n)),ceiling(.5*n),min(n,ceiling(.9*n)+1))]})
   print(quantiles)
   par(mfrow=c(1,1))
   par(mai=c(0,0,0,0),omi=c(0.8,1.0,0.6,0.1))
   par(cex.axis=.8)
   
   K <- min(n,n.worms)
   #find some barely distinguishable gray-black lines for the worms
   plot.col.palette<-colorRampPalette(c("gray50","black"))  #very cool function to create a new palette
   plot.cols <- plot.col.palette(n=K)[1:K]

   worms.index <- sample(1:n,size=K)
   if(missing(ylim)) ylim <- c(0,max(tmp[worms.index,],quantiles))
   else ylim <- ylim

   plot(yr.first:yr.last,quantiles[2,],ylim=ylim,xlab="",ylab="",
                    xlim=c(yr.first,yr.last),xaxt="n",cex=1,yaxs="i",xaxs="i",las=1)

   polygon(c(yr.first:yr.last,yr.last:yr.first),c(quantiles[1,],rev(quantiles[3,])),col=13,border=NA) 
   lines(yr.first:yr.last,quantiles[2,],type="o",lwd=2,col="red")
   if(K>0) { 
     for(i in 1:K)  {
       lines(yr.first:yr.last,tmp[worms.index[i],],col=plot.cols[i])
     }
   }
   axis(side=1,cex=1)
   mtext("Year",side=1,outer=T,cex=1.3,line=2.5)
   if (withError)
   {
     mtext("Simulated aerial survey index",side=2,outer=T,cex=1.3,line=3)
   }
   else
   {
     mtext("Predicted aerial survey index",side=2,outer=T,cex=1.3,line=3)
   }
   if (!is.null(title)) {
       mtext(title,side=3,outer=T,line=1,cex=1)
   }
   box()
   invisible(tmp)
}
