#' Plot.fromMSYstats.R(MSY.stats,use.B10=FALSE,catch.over.B=FALSE)
#' 
#' Plots U/Umsy vs B/Bmsy (Kobe) over time with confidence intervals. 
#' and other MSY plots
#' NB! first run MSY.stats <- calc.MSYstuff.R(base,lev.file="base.lev")
#' use.B10=FALSE will plot SSB, otherwise plot B10plus
#' Umsy and Bmsy were calculated by msycalc.tpl using anual weights, selectivities and allocation
#' data.objects is the collection of lab.rep files produced by get.all.data.R()
#' catch.over.B = T plots U = catch/Total biomass divided by Umsy=MSY/TBmsy
#' otherwise plot average F over ages 2-15 weigthed by biomass.
#' There may be false convergences in msycalc which are replaced by NA.
#' to check use:  apply(MSY.stats$output$MSY,2,countNAs)   (countNAs is length(x[is.na(x)])
#' Ana Parma & Jim Ianelli (modified from 2011 Bali function)
#'   library(gplots) 
#' 
#' @param y the function input
#' @return something done on y
#' @author Ana Parma
#' @examples
#' Plot.MSYstuff.R(MSY.stats)
#' @export
#'
Plot.fromMSYstats.R <- function(MSY.stats,use.B10=F,catch.over.B=F,xlim=c(0,4),ylim=NULL,
  plt_col=F,show_arrow=F,show_quants=T,plot_out=NULL) 
{
   years <- as.numeric(colnames(MSY.stats$B.Bmsy.quantiles))
   nyears <- length(years)
   if(use.B10==FALSE)
   {
         Bquantiles<- MSY.stats$B.Bmsy.quantiles 
         BBmsy <- MSY.stats$SSB/MSY.stats$SSBmsy
   }
   else     # use B10plus in plots
   {
         Bquantiles<- MSY.stats$B10.B10msy.quantiles 
         BBmsy <- MSY.stats$B10plus/MSY.stats$B10msy
   }
   if(catch.over.B==TRUE) 
   { 
       Uquantiles = MSY.stats$UUmsyquantiles 
       U = MSY.stats$UUmsy
       ylabel = "U/Umsy"
   }
   if(catch.over.B==FALSE) 
   {
      Uquantiles = MSY.stats$FFmsyquantiles
      U = MSY.stats$FFmsy
      ylabel = "Ratio F/Fmsy (average ages 2-15)"
   }
   # KOBE plot
   Bmed <- Bquantiles["50%",]
   Fmed <- Uquantiles["50%",]
   if (is.null(ylim) )
    ylim=c(0,round(1.05*max(Fmed),1))

  if (!is.null(plot_out)) 
  {
    if (plot_out=="pdf")
      pdf(file="kobe%03d.pdf")
    if (plot_out=="png")
      png(file="kobe%03d.png")
  }
  par(mar=c(7,6,2,3))
  plot(Bmed,Fmed,type="n",xaxs="i",yaxs="i",
        ylim=ylim,
        xlim=xlim,
        ylab=ylabel,xlab="B/Bmsy")
       
# plot(c(0,0), c(0,0), ylim=c(0,2.2), xlim=c(0,3.5), type="n", ylab="", xlab="")
  ymax=max(ylim)
  xmax=max(xlim)
  polygon(c(0,1,1,0,0), c(1,1,ymax,ymax,1), col="#FF6A6A44")
  polygon(c(1,xmax,xmax,1,1), c(1,1,ymax,ymax,1), col="#FFF68F85" )
  polygon(c(1,xmax,xmax,1,1), c(0,0,1,1,0), col="#CAFF7083")
  polygon(c(0,1,1,0,0), c(0,0,1,1,0), col="#FFF68F25")
  lines(c(0,xmax), rep(1,2), lwd=6)
  lines(c(1,1), c(0,ymax), lwd=6)

  if (plt_col)
  {
    plot.col.palette<-colorRampPalette(c("green","blue"))
    plot.colors <- plot.col.palette(n=nyears-1)
  }
  else
  {
    plot.col.palette<-colorRampPalette(c("grey","black"))
    plot.colors <- plot.col.palette(n=nyears-1)
  }
  abline(v=1,h=1,lty=2,lwd=2,col="grey50")
  if (show_arrow)
    arrows(x0=Bmed[-nyears], x1=Bmed[-1],y0=Fmed[-nyears],y1=Fmed[-1], las=1, yaxs="i", length=0.13, lwd=3.5, col=plot.colors) 
  else
    arrows(x0=Bmed[-nyears], x1=Bmed[-1],y0=Fmed[-nyears],y1=Fmed[-1], las=1, yaxs="i", length=0.00, lwd=3.5, col=plot.colors) 
 
  if (show_quants)
  {
    for (i in 1:nyears)
    {
      xx=rep(Bquantiles["50%",i],2)
      yy=Uquantiles[c("25%","75%"),i]
      lines(xx,yy)
      yy=rep(Uquantiles["50%",i],2)
      xx=Bquantiles[c("25%","75%"),i]
      lines(xx,yy,lwd=1,col=plot.colors[i])
    }
  }
  text(Bmed[nyears],Fmed[nyears],years[nyears],cex=1.5,col="grey10")
  text(Bmed[nyears-9],Fmed[nyears-9],years[nyears-9],cex=1.5,col="grey10")
  text(Bmed[nyears-20],Fmed[nyears-20],years[nyears-20],cex=1.5,col="grey10")
  text(Bmed[nyears-45],Fmed[nyears-45],years[nyears-45],cex=1.5,col="grey10")
  text(Bmed[5],Fmed[5],years[5],cex=1.5,col="grey10")
 #### boxplots over time
  if (is.null(plot_out)) 
  {
    win.graph(width=11.5,height=11.5)
  }
  else
  {
    dev.off()
    if (plot_out=="pdf")
      pdf(file="F_Fmsy%03d.pdf")
    if (plot_out=="png")
      png(file="F_Fmsy%03d.png")
  }
  boxplot(U,names=years,cex=0.4,col=3)
  abline(h=1,lty=2)
  mtext(ylabel,3,1)

  if (is.null(plot_out)) 
  {
    win.graph(width=11.5,height=11.5)
  }
  else
  {
    dev.off()
    if (plot_out=="pdf")
      pdf(file="B_Bmsy%03d.pdf")
    if (plot_out=="png")
      png(file="B_Bmsy%03d.png")
  }
  boxplot(BBmsy,names=years,cex=0.4,col=3)
  abline(h=1)
  mtext("Ratio of Bt over Bmsy",3,1)
 
  if (is.null(plot_out)) 
  {
    win.graph(width=11.5,height=11.5)
  }
  else
  {
    dev.off()
    if (plot_out=="pdf")
      pdf(file="MSY%03d.pdf")
    if (plot_out=="png")
      png(file="MSY%03d.png")
  }
  boxplot(MSY.stats$MSY,names=years,cex=0.4,col=3,main="MSY")

  if (!is.null(plot_out)) dev.off()
}

