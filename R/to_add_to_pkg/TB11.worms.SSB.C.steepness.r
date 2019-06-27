#' Plot SSB and catch worms by steepness values
#' 
#' Function to plot median biomass and catch projections for one MP and  one operating model, as well as the 80% confidence interval; can also include any number of individual realizations (i.e. worms)
#' 
#' @param s3filename name of the .s3 filename including the path from the R file
#' @param yaxis.B a vector giving the lower and upper bound to use for the y-axis of the biomass plot (eg. c(0,100)); if missing, the program automatically calculates bounds
#' @param yaxis.C a vector giving the lower and upper bound to use for the y-axis of the catch plot (eg. c(0,50)); if missing, the program automatically calculates bounds
#' @param seed integer specifying the seed for the random number generator (thus allowing for the same realizations to be compared between plots) (default=1)
#' @param h.level is steepness run number, usually 1, 2, 3, 4, or 5 corresponding to steepness h=0.55, etc.
#' @export
#' 
worm.SSB.C.steepness.f <- function(s3filename=".\\CONST\\CR0\\CONST_c0s0l0_0.s3",n.worms=10,yaxis.B, yaxis.C, seed=1, title="", translucent=F, h.level=1)
{
  set.seed(seed)
  if (translucent==F) { line.col <- "black"} else { line.col <- "#00000022" }

  #.Options$warn <- -1  # suppress annoying warning messages
  tmp <- read.table(s3filename,skip=2,header=T)
  tmp <- as.matrix(tmp[,-c(1,2)])
  nyr <- (ncol(tmp)-1)/2
  h.rows <- (substr(tmp[,2],1,1)==h.level)  #True for all rows where first number is equal to h.level
  
  #auto-find first year using column headers in s3 file
  yr.first <- 2006
  while(length(grep(yr.first,colnames(tmp)))==0) {
     yr.first <- yr.first+1     
  }
  #yr.first <- 2006
  yr.last <- yr.first+nyr

  bworms <- tmp[h.rows,1:(1+nyr)] / 1e06
  cworms <- tmp[h.rows,(2+nyr):(2*nyr+1)] / 1e03

  #B.quantiles<- apply(bworms,2,function(x){quantile(x,c(.1,.5,.9))})
  #C.quantiles<- apply(cworms,2,function(x){quantile(x,c(.1,.5,.9))})

  # Using the quantile function does not give the exact same anwer as Vivian
  # gets in her summary files; instead calculate quantiles the following way:
  n <- nrow(bworms)
  B.quantiles<- apply(bworms,2,function(x){sort(x)[c(max(1,trunc(.1*n)),ceiling(.5*n),min(n,ceiling(.9*n)+1))]})
  C.quantiles<- apply(cworms,2,function(x){sort(x)[c(max(1,trunc(.1*n)),ceiling(.5*n),min(n,ceiling(.9*n)+1))]})

  par(mfrow=c(2,1), mai=c(0,1,0,.2),omi=c(0.8,0,0.6,0),cex.axis=.8)

  K <- min(nrow(bworms),n.worms)
  worms.index <- sample(1:nrow(bworms),K)

  if(missing(yaxis.B)) ylim <- c(0,max(bworms[worms.index,],B.quantiles))
  else ylim <- yaxis.B
  plot(yr.first:yr.last,B.quantiles[2,],ylim=ylim,
    xlab="",ylab=expression(paste("Spawning biomass (",10^6," mt)",sep="")),
    xlim=c(yr.first-.5,yr.last+.5),xaxt="n",cex=1, xaxs="i",yaxs="i",las=1)
  polygon(c(yr.first:yr.last,yr.last:yr.first),c(B.quantiles[1,],
    rev(B.quantiles[3,])),col=13,border=NA) 

  if(n.worms>0) { 
    for(i in worms.index)  {
      lines(yr.first:yr.last,bworms[i,], col=line.col)
    }
    for(i in worms.index)  {
      if (min(bworms[i,])<0.01) {
        lines(yr.first:yr.last,bworms[i,], col="yellow",lwd=2)
      }
    }
  }

  lines(yr.first:yr.last,B.quantiles[2,],type="o",lwd=2,col="blue")

  mtext(title,side=3,outer=F,line=1,cex=1)

  if(missing(yaxis.C)) ylim <- c(0,max(cworms[worms.index,],C.quantiles))
  else ylim <- yaxis.C
  plot(yr.first:(yr.last-1),C.quantiles[2,],ylim=ylim,
        xlab="",ylab=expression(paste("Catch (",10^3," mt)",sep="")),
        xlim=c(yr.first-.5,yr.last+.5),xaxt="n",cex=1, xaxs="i",yaxs="i",las=1)
  polygon(c(yr.first:(yr.last-1),(yr.last-1):yr.first),c(C.quantiles[1,],
    rev(C.quantiles[3,])),col=13,border=NA) 
  if(n.worms>0) { 
    for(i in worms.index)  {
      lines(yr.first:(yr.last-1),cworms[i,], col=line.col)
    }
    for(i in worms.index)  {
      if (min(cworms[i,])<0.5) {
        lines(yr.first:(yr.last-1),cworms[i,], col="yellow",lwd=2)
      }
    }
  }
  lines(yr.first:(yr.last-1),C.quantiles[2,],type="o",lwd=2,col="blue")
  axis(side=1,cex=1)
  mtext("Year",side=1,outer=F,cex=1,line=2.5)

  invisible(tmp)
}
