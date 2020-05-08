#' @title Plot median biomass and catch projections for one MP and one operating model
#' @description
#' Function to plot median biomass and catch projections for one MP and one operating model, as well as the 80% confidence interval; can also include any number of individual realizations (i.e. worms)
#' @export
#' 
worm.SSB.C.multiples.f <- function(s3filename, s4filename,n.worms=10,yaxis.B, yaxis.C,
  seed=1, title="", translucent=F, first=T, plot.02B0=F)
{
#Arguments to function are:
#   s3filename: name of the .s3 filename including the path from the R file
#5) yaxis.B: a vector giving the lower and upper bound to use for the y-axis of the 
#     biomass plot (eg. c(0,100)); if missing, the program automatically calculates bounds
#6) yaxis.C: a vector giving the lower and upper bound to use for the y-axis of the 
#     catch plot (eg. c(0,50)); if missing, the program automatically calculates bounds
#7) seed: integer specifying the seed for the random number generator (thus allowing for the same
#     realizations to be compared between plots) (default=1)

  set.seed(seed)
  if (translucent==F) { line.col <- "black"} else { line.col <- "#00000022" }

  #.Options$warn <- -1  # suppress annoying warning messages
  tmp <- read.table(s3filename,skip=2,header=T)
  tmp <- as.matrix(tmp[,-c(1,2)])
  nyr <- (ncol(tmp)-1)/2
  
  #auto-find first year using column headers in s3 file
  yr.first <- 2006
  while(length(grep(yr.first,colnames(tmp)))==0) {
     yr.first <- yr.first+1     
  }
  #yr.first <- 2006
  yr.last <- yr.first+nyr

  bworms <- tmp[,1:(1+nyr)] / 1e06
  cworms <- tmp[,(2+nyr):(2*nyr+1)] / 1e03

  #B.quantiles<- apply(bworms,2,function(x){quantile(x,c(.1,.5,.9))})
  #C.quantiles<- apply(cworms,2,function(x){quantile(x,c(.1,.5,.9))})

  # Using the quantile function does not give the exact same anwer as Vivian
  # gets in her summary files; instead calculate quantiles the following way:
  n <- nrow(bworms)
  B.quantiles<- apply(bworms,2,function(x){sort(x)[c(max(1,trunc(.1*n)),ceiling(.5*n),min(n,ceiling(.9*n)+1))]})
  C.quantiles<- apply(cworms,2,function(x){sort(x)[c(max(1,trunc(.1*n)),ceiling(.5*n),min(n,ceiling(.9*n)+1))]})

  #par(mfrow=c(2,1), mai=c(0,1,0,.2),omi=c(0.8,0,0.6,0),cex.axis=.8)

  K <- min(nrow(bworms),n.worms)
  worms.index <- sample(1:nrow(bworms),K)

  if(missing(yaxis.B)) ylim <- c(0,max(bworms[worms.index,],B.quantiles))
  else ylim <- yaxis.B
  if (first==T) {
     ylab <- expression(paste("Spawning biomass (",10^6," mt)",sep=""))
     yaxt="s"
  }
  else{
     ylab <- ""
     yaxt="n"
  }
  plot(yr.first:yr.last,B.quantiles[2,],ylim=ylim,
	 xlab="",ylab=ylab,
	 xlim=c(yr.first-.5,yr.last+.5),xaxt="n",yaxt=yaxt,cex=1, xaxs="i",yaxs="i",las=1)
  polygon(c(yr.first:yr.last,yr.last:yr.first),c(B.quantiles[1,],
    rev(B.quantiles[3,])),col=13,border=NA) 
  mtext(outer=F,side=2,line=2.5,ylab)

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
  if (plot.02B0==T) {   #add dashed abline at 20% SSB0
      tmp4 <- read.table(s4filename,skip=2,header=T)
      SSB0.col <- grep("B.1931",colnames(tmp4))
      plot.val <-median(0.2*tmp4[,SSB0.col])
      print(plot.val)
      abline(h=plot.val/1e06,lty=2)
  }

  lines(yr.first:yr.last,B.quantiles[2,],type="o",lwd=2,col="blue")

  mtext(title,side=3,outer=F,line=1,cex=1)

  if(missing(yaxis.C)) ylim <- c(0,max(cworms[worms.index,],C.quantiles))
  else ylim <- yaxis.C
  if (first==T) {
     ylab <- expression(paste("Catch (",10^3," mt)",sep=""))
  }
  else{
     ylab <- ""
  }
  plot(yr.first:(yr.last-1),C.quantiles[2,],ylim=ylim,
        xlab="",ylab="",
        xlim=c(yr.first-.5,yr.last+.5),xaxt="n",yaxt=yaxt,cex=1, xaxs="i",yaxs="i",las=1)
  mtext(outer=F,side=2,line=2.5,ylab)
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
#####Fig. 16 Worm plots showing both SSB and catch in the future
###source("TB10.worms.SSB.Catch.r")
#win.graph(height=11.5,width=8)
#x <- worm.SSB.C.f(s3filename=".\\CURRENT\\v0\\CURRENT_c1s1l13h.s3",n.worms=10,yaxis.B=c(0,0.6), yaxis.C=c(0,11.9),
#  seed=1, title="Projections: constant 2011 catches mt")
