#' Plot.MSYstuff.R(MSY.stats)
#' 
#' Calls Plot.Kobe to plot U/Umsy vs B/Bmsy over time with confidence intervals. 
#' NB! first run MSY.stats <- calc.MSYstuff.R(base,lev.file="base.lev")
#' Umsy and Bmsy were calculated by msycalc.tpl using anual weights, selectivities and allocation
#' data.objects is the collection of lab.rep files produced by get.all.data.R()
#' catch.over.B = T plots U = catch/Total biomass divided by Umsy=MSY/TBmsy
#' otherwise plot average F over ages 2-15 weigthed by biomass.
#' NB: there may be false convergences which are replaved by NA.
#' to check use:  apply(MSY.stats$output$MSY,2,countNAs)   (countNAs is length(x[is.na(x)])
#' Jim Ianelli & Ana Parma, modified from Bali 2011
#' 
#' @param y the function input
#' @return something done on y
#' @author Ana Parma
#' @examples
#' MSY.stats <- plot.MSYstuff.R(base25)
#' @export
#'

