#' @title Panel histogram.
#' @description
#' Plot shaded plots. Put histograms on the diagonal.
#' @author Jim Ianelli.
#' @export
#' 
panel_hist <- function(x, ...)
{
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(usr[1:2], 0, 1.5) )
    h <- hist(x,plot = FALSE,breaks=1:7 -0.5)
    breaks <- h$breaks; nB <- length(breaks)
    
    y <- h$counts; y <- y/max(y)
    rect(breaks[-nB], 0, breaks[-1], y, col="green",bg="green", ...)
}


#' @title Panel smooth.
#' @description
#' Panel smooth.
#' @title Panel smooth.
#' @author Jim Ianelli.
#' @export
#' 
panel_smooth <- function (x, y, col = par("col"), bg = "green", pch = 24, cex = 1, col.smooth = "red", span = 1./3, iter = 3, ...) 
{
    points(x, y, pch = pch, col = col, bg = bg, cex = cex)
    ok <- is.finite(x) & is.finite(y)
    if (any(ok)) 
        lines(stats::lowess(x[ok], y[ok], f = span, iter = iter), col = col.smooth, lw=2, ...)
}


#' @title Panel correlations.
#' @description
#' Plot shaded plots. Put (absolute) correlations on the upper panels, with size proportional to the correlations.
#' @author Jim Ianelli.
#' @export
#' 
panel_cor <- function(x, y, digits = 2, prefix = "", cex.cor)
{
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(0, 1, 0, 1))
    r <- (cor(x, y))
    txt <- format(c(r, 0.123456789), digits=digits)[1]
    txt <- paste(prefix, txt, sep="")
    if(missing(cex.cor)) cex <- 0.8/strwidth(txt)
    text(0.5, 0.5, txt, cex = cex * abs(r))
}


#' @title Plot lev
#' @author Jim Ianelli
#' @export
#' 
plot_lev <- function(file, factor = 2.3, title = NULL, drop_if_one_level = TRUE)
{
   d <- read.table(file)
   names(d) <- c("Steep","M0","M10","Omega","CPUE","q_AgeRng","Psi","SampleSz")
   if (drop_if_one_level)
   {
     uniquelength <- sapply(d, function(x) length(unique(x)))
     d <- subset(d, select = uniquelength > 1)
   }
   dd <- apply(d, MARGIN = 2, FUN = jitter, factor = factor)
   if (is.null(title)) { title <- file }
   pairs(dd, main = title, cex = 0.2, lower.panel = panel_smooth, diag.panel = panel_hist, cex.labels = 1.1, font.labels = 2)
}
