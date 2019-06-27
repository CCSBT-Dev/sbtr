#' Completely eviscerated version of filled.contour()
#' 
#' require(KernSmooth); require(MASS); require(gregmisc)  # this is the one that has filled.contour
#' 
#' @export
#' 
filled.contour.TAB <- function (x = seq(0, 1, length.out = nrow(z)), y = seq(0, 1, 
    length.out = ncol(z)), z, xlim = range(x, finite = TRUE), 
    ylim = range(y, finite = TRUE), zlim = range(z, finite = TRUE), 
    levels = pretty(zlim, nlevels), nlevels = 20, color.palette = cm.colors, 
    col = color.palette(length(levels) - 1), plot.title, plot.axes, 
    key.title, key.axes, asp = NA, xaxs = "i", yaxs = "i", las = 1, 
    axes = TRUE, frame.plot = axes, ...) 
{
    if (missing(z)) {
        if (!missing(x)) {
            if (is.list(x)) {
                z <- x$z
                y <- x$y
                x <- x$x
            }
            else {
                z <- x
                x <- seq.int(0, 1, length.out = nrow(z))
            }
        }
        else stop("no 'z' matrix specified")
    }
    else if (is.list(x)) {
        y <- x$y
        x <- x$x
    }
    if (any(diff(x) <= 0) || any(diff(y) <= 0)) 
        stop("increasing 'x' and 'y' values expected")
     plot.new()
    plot.window(xlim, ylim, "", xaxs = xaxs, yaxs = yaxs, asp = asp)
    if (!is.matrix(z) || nrow(z) <= 1 || ncol(z) <= 1) 
        stop("no proper 'z' matrix specified")
    if (!is.double(z)) 
        storage.mode(z) <- "double"
    .Internal(filledcontour(as.double(x), as.double(y), z, as.double(levels), 
        col = col))
    invisible()
}


#' Plot of B/Bmsy against F/Fmsy for the most recent year
#' 
#' @author Trevor A. Branch
#' @export
#' 
Plot.phase.plane <- function(BoverBmsy, FoverFmsy,xlim,ylim,header,bw.mult=1,jitter.fac=0)
{
   #plot(x=BoverBmsy,y=FoverFmsy,xlim=xlim,ylim=ylim,las=1, yaxs="i",xaxs="i",xlab="",ylab="")
   crosshair.data.uncen <- cbind(BoverBmsy,FoverFmsy)
   #APRIL 22 version: References in Scott 1992 and Bowman and Azzalini 1997
   d<-2 # the bandwidth dimension
   bmsy.bw<-sqrt(var(crosshair.data.uncen[,1]))*(4/((d+2)*length(crosshair.data.uncen[,1])))^(1/(d+4))
   umsy.bw<-sqrt(var(crosshair.data.uncen[,2]))*(4/((d+2)*length(crosshair.data.uncen[,2])))^(1/(d+4))
   # please note the range restrictions at 2.01 to include the points that line up at the boundaries
   kernel.dens <- bkde2D(crosshair.data.uncen[,c(1,2)], bandwidth=c(bmsy.bw*bw.mult,umsy.bw*bw.mult), range.x=list(xlim,ylim))
   # generate color palette 
   paletteable.egg<-colorRampPalette(c("#BFEFFF","white","white", "yellow","#FFC125"))
   filled.contour.TAB(kernel.dens$x1, kernel.dens$x2, kernel.dens$fhat, nlevels=15, color.palette =paletteable.egg,
               xlab="", ylab="", xlim=xlim, ylim=ylim, cex.lab=1.3)
   par(new=T)
   plot(x=jitter(BoverBmsy,jitter.fac),y=jitter(FoverFmsy,jitter.fac),xlim=xlim,ylim=ylim,las=1,
          yaxs="i",xaxs="i",xlab="",ylab="",col="gray50",pch=20)
   mtext(side=1,outer=F,line=3.2,expression(B/B[MSY]),cex=1.3)
   mtext(side=2,outer=F,line=3,expression(F/F[MSY]),cex=1.3)
   mtext(side=3,outer=F,line=0.5,header,cex=1.3)
   abline(h=1,lty=2)
   abline(v=1,lty=2)
}
