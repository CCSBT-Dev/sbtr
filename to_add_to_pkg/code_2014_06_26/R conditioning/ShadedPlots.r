####################################################
#Code by Jim Ianelli. Plots shaded plots 
####################################################

## put histograms on the diagonal
panel.hist <- function(x, ...)
{
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(usr[1:2], 0, 1.5) )
    h <- hist(x,plot = FALSE,breaks=1:7 -0.5)
    breaks <- h$breaks; nB <- length(breaks)
    
    y <- h$counts; y <- y/max(y)
    rect(breaks[-nB], 0, breaks[-1], y, col="green",bg="green", ...)
}

panel.smooth <- function (x, y, col = par("col"), bg = "green", pch = 24, cex = 1, col.smooth = "red", span = 1./3, iter = 3, ...) 
{
    points(x, y, pch = pch, col = col, bg = bg, cex = cex)
    ok <- is.finite(x) & is.finite(y)
    if (any(ok)) 
        lines(stats::lowess(x[ok], y[ok], f = span, iter = iter), 
            col = col.smooth, lw=2, ...)
}

## put (absolute) correlations on the upper panels,
## with size proportional to the correlations.
panel.cor <- function(x, y, digits=2, prefix="", cex.cor)
{
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(0, 1, 0, 1))
    r <- (cor(x, y))
    txt <- format(c(r, 0.123456789), digits=digits)[1]
    txt <- paste(prefix, txt, sep="")
    if(missing(cex.cor)) cex <- 0.8/strwidth(txt)
    text(0.5, 0.5, txt, cex = cex * abs(r))
}

plot.lev <- function(file, factor=2.3,title=NULL)
{
   d<<-read.table(file)
   names(d)<<-c("Steep","M0","M10","Omega","CPUE","q_AgeRng","SampleSz" )
   dd<-apply(d,MARGIN=2,FUN=jitter,factor=factor)
   if (is.null(title)) {  title<-file  }
   pairs(dd, main=title,cex=.2, lower.panel=panel.smooth,diag.panel=panel.hist, cex.labels=1.1, font.labels=2) 
}
#win.graph(height=11.5,width=11.5)
#plot.lev("C1S1L1.lev")

#make a 150KB file instead of 1.5 MB file
#jpeg(file="figs\\ScatterGrid.jpg", width=600,height=600)
#plot.lev("levfiles\\C1S1L1.lev")
#dev.off()
#pdf("figs//ScatterGrid.pdf",height=11.5,width=11.5)
#plot.lev("C1S1L1.lev")
#dev.off()
