#' MSY.stats <- plot.MSYstuff.R(base25)
#' 
#' Plots U/Umsy vs B/Bmsy over time with confidence intervals. 
#' Umsy and Bmsy were calculated by msycalc.tpl using anual weights, selectivities and allocation
#' data.objects is the collection of lab.rep files produced by get.all.data.R()
#' catch.over.B = T plots U = catch/Total biomass divided by Umsy=MSY/TBmsy
#' otherwise plot average F over ages 2-15 weigthed by biomass.
#' NB: there may be false convergences which are replaved by NA.
#' to check use:  apply(MSY.stats$output$MSY,2,countNAs)   (countNAs is length(x[is.na(x)])
#' Ana Parma, Bali 2011
#'   library(gplots) 
#' 
#' @param y the function input
#' @return something done on y
#' @author Ana Parma
#' @examples
#' MSY.stats <- plot.MSYstuff.R(base25)
#' @export
#'
Plot.MSY.cov <- function(data.objects,lev.file="base.lev", cov_sims="sims",
  catch.over.B=FALSE,xlim=c(0,4),ylim=NULL,
  plt_col=F,show_arrow=F,plot_out=NULL) 
{
   xx       <- data.objects[[1]]
   years    <- xx$yrs.catch[1]:xx$yrs.catch[2] 
   nyears   <- length(years)
   nobjects <- length(data.objects)
   allyears <- xx$years[1]:(xx$years[2]+1)
   id.years <- allyears %in% years   #for selecting years with catch > 0

   TOTcatch <- apply(xx$catch.weight.pred,2,sum)
    
   var.names<- c("scenario","Sbio","TOTbio","MSY","TBmsy","SSBmsy","Fmsy","F","BBmsy","UUmsy","FFmsy","SSB0")
   output   <- vector("list",length(var.names))
   names(output)   <- var.names
   output$Sbio     <- matrix(nrow=nobjects,ncol=nyears)  #nyears is only for catch years
   output$TOTbio   <- matrix(nrow=nobjects,ncol=nyears)
   output$UUmsy    = matrix(nrow=nobjects,ncol=nyears)
   output$FFmsy    = matrix(nrow=nobjects,ncol=nyears)
   output$BBmsy    = matrix(nrow=nobjects,ncol=nyears)
   output$F        = matrix(nrow=nobjects,ncol=nyears)
   output$MSY      = matrix(nrow=nobjects,ncol=nyears)
   output$TBmsy    = matrix(nrow=nobjects,ncol=nyears)
   output$SSBmsy   = matrix(nrow=nobjects,ncol=nyears)
   output$Fmsy     = matrix(nrow=nobjects,ncol=nyears)
    
   output$B10msy   = matrix(nrow=nobjects,ncol=nyears)
   output$B10      = matrix(nrow=nobjects,ncol=nyears)
   
   output$SSB0     = rep(NA,nobjects)
   output$scenario = rep(NA,nobjects)
 
   for (i in 1:nobjects)
   {
       xx <- data.objects[[i]]
      output$scenario[i]=xx$scenario_number
      output$Sbio[i,]=xx$Sbio[id.years]
      output$TOTbio[i,]=xx$TOTbio[id.years]
      output$MSY[i,]=xx$MSY          # xx$MSY is a vector in the dynamic version
      output$Fmsy[i,] = xx$Fmsy_a215
      output$TBmsy[i,]=xx$TBmsy   #total biomass
      output$SSBmsy[i,]=xx$Bmsy  #spawning stock biomass
      output$UUmsy[i,] = (TOTcatch/xx$TOTbio[id.years])/(xx$MSY/xx$TBmsy)      
      output$F[i,] = xx$F_a215
      output$SSB0[i] = xx$B0
      output$B10[i,]=xx$B10_plus[id.years]
      output$B10msy[i,] =  apply(xx$Bmsy_a[,11:31],1,sum)
   }       
  output$BBmsy =  output$Sbio/output$SSBmsy
  output$B10B10msy =  output$B10/output$B10msy
  output$FFmsy =  output$F/output$Fmsy
  fits=output

   #check convergence and set to NA
   id.wrong <-  output$MSY < 1
   output$MSY[id.wrong]=NA      
   output$Fmsy[id.wrong]=NA
   output$SSBmsy[id.wrong]=NA   
   output$TBmsy[id.wrong]=NA   
   output$UUmsy[id.wrong]=NA   
   output$BBmsy[id.wrong]=NA    
   output$B10B10msy[id.wrong]=NA    
 

 # sample over grid    
    lev <- read.table(file=lev.file, colClasses="numeric", sep=" ")
    nlevs = nrow(lev)    # 
    lev.scens <- as.numeric(paste(lev[,1],lev[,2],lev[,3],lev[,4],lev[,5],lev[,6],sep =""))
    resamps <- match(lev.scens,output$scenario)
   MSY          = output$MSY[resamps,]
   Fmsy         = output$Fmsy[resamps,]
   SSBmsy       = output$SSBmsy[resamps,]     
   B10msy       = output$B10msy[resamps,]     
   TBmsy        = output$TBmsy[resamps,]             
   Sbio         = output$Sbio[resamps,]
   B10          = output$B10[resamps,]
   TOTbio       = output$TOTbio[resamps,]
   FFmsy        = output$FFmsy[resamps,]
   UUmsy        = output$UUmsy[resamps,]
   BBmsy        = output$BBmsy[resamps,]
   B10B10msy    = output$B10B10msy[resamps,]
   scenario     = output$scenario[resamps]
   B0           = output$SSB0[resamps]
   B10B10within = cov_sims$B10/B10msy

   myquants        = c(0.05,0.1,0.5,0.9,0.95)
   Bquantiles      = apply(BBmsy,2,quantile,myquants,na.rm=T)
   B10quantiles    = apply(B10B10msy,2,quantile,myquants,na.rm=T)
   B10within_quant = apply(B10B10within,2,quantile,myquants,na.rm=T)

  plot(years,B10quantiles[3,],ylim=c(0,max(B10quantiles[,4])), xlab="",ylab="",cex.lab=1.8,cex.axis=1.3,
   cex=.6, xaxs="i",yaxs="i",las=1,type="b",lwd=1,pch=19,
   main="Comparison of within-cell variability, B/Bmsy")
  abline(h=1)
  col1 = "#4682B433"
  col1 = "#FF7F5053"
  col2 = "#CAFF7086"
  add.polygon(years,B10within_quant[2,],B10within_quant[4,],col=col1)
  add.polygon(years,B10quantiles[2,],B10quantiles[4,],col=col2)
  legend(1990,3,c("Grid-cell only","Include within-cell estimation error"),pch=c(15,15),pt.cex=2,col=c(col2,col1))
  points(years,B10within_quant[3,], cex=.6, xaxs="i",yaxs="i",las=1,type="l")

#   colnames(Bquantiles) <-  

   if(catch.over.B==TRUE) 
   { 
       Uquantiles = apply(UUmsy,2,quantile,na.rm=T)
       U          = UUmsy
       ylabel     = "U/Umsy"
       }
   if(catch.over.B==FALSE) 
   {
      Uquantiles = apply(FFmsy,2,quantile,na.rm=T)
      U          = FFmsy
      ylabel     = "Ratio F/Fmsy (average ages 2-15)"
   }

  if (!is.null(plot_out)) 
  {
    if (plot_out=="pdf")
      pdf(file="kobe%03d.pdf")
    if (plot_out=="png")
      png(file="kobe%03d.png")
  }
  if (!is.null(plot_out)) dev.off()
 
 return(list(
   "SSBquantiles"        = Bquantiles,
   "B10quantiles"        = B10quantiles,
   "B10within_quantiles" = B10within_quant 
   ))
}



