#' MSY.stats <- calc.MSYstuff.R(base,lev.file="base16sqrt.lev",sample=T)
#'
#' calculates U/Umsy , B/Bmsy B10/B10msy and B/B0 over time with confidence intervals.
#' Umsy and Bmsy were calculated by msycalc.tpl using anual weights, selectivities and allocation
#' data.objects is the collection of lab.rep files produced by get.all.data.R()
#' NB: there may be false convergences in msycalc which are replaved by NA.
#' to check use:  apply(MSY.stats$output$MSY,2,countNAs)   (countNAs is length(x[is.na(x)])
#' Ana Parma, 2014  (extracted and modified from plot.MSYstuff.R)
#'
#' @param y the function input
#' @return something done on y
#' @author Ana Parma
#' @export
#'
calc.MSYstuff.R <- function(data.objects, lev.file = "base.lev", sample = T) {
   xx <- data.objects[[1]]
   years <- xx$yrs.catch[1]:(xx$yrs.catch[2]+1)
   nyears <- length(years)
   nobjects <- length(data.objects)
   allyears <- xx$years[1]:(xx$years[2]+1)
   id.years <- allyears %in% years # for selecting years with catch > 0 plus last year

   TOTcatch <- apply(xx$catch.weight.pred,2,sum)

   var.names <- c("scenario","SSB","TOTbio","B10", "MSY","TBmsy","SSBmsy","B10msy","Fmsy","F","BBmsy","UUmsy","FFmsy","SSB0")
   mm <- matrix(nrow = nobjects, ncol = nyears)  #nyears is number of years with catch
   colnames(mm) <- years
   # output=vector("list",length(var.names))
   output <- rep(list(mm),length(var.names))   #create a list of matrices
   names(output) <- var.names
   # replace these two by vectors
   output$SSB0 <- rep(NA, nobjects)
   output$scenario <- rep(NA, nobjects)

   for (i in 1:nobjects) {
      xx <- data.objects[[i]]
      output$scenario[i] = xx$scenario_number
      output$SSB[i,] = xx$Sbio[id.years]
      output$TOTbio[i,] = xx$TOTbio[id.years]
      output$MSY[i,-nyears] = xx$MSY          # xx$MSY is a vector in the dynamic version
      output$Fmsy[i,-nyears] = xx$Fmsy_a215
      output$TBmsy[i,-nyears] = xx$TBmsy   #total biomass
      output$SSBmsy[i,-nyears] = xx$Bmsy  #spawning stock biomass
      output$UUmsy[i,-nyears] = (TOTcatch/output$TOTbio[i,-nyears])/(xx$MSY/xx$TBmsy)
      output$F[i,-nyears] = xx$F_a215
      output$SSB0[i] = xx$B0
      output$B10[i,]=xx$B10_plus[id.years]
      output$B10msy[i,-nyears] =  apply(xx$Bmsy_a[,11:31],1,sum)
   }
   output$BBmsy =  output$SSB / output$SSBmsy
   output$B10B10msy =  output$B10 / output$B10msy
   output$FFmsy =  output$F / output$Fmsy

   fits=list(output$MSY, output$Fmsy, output$SSBmsy, output$TBmsy)
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
   if (sample) {
     lev <- read.table(file = lev.file, colClasses = "numeric", sep = " ")
     nlevs = nrow(lev)
     lev.scens <- as.numeric(paste(lev[,1],lev[,2],lev[,3],lev[,4],lev[,5],lev[,6],lev[,7],sep=""))
     resamps <- match(lev.scens,output$scenario)
   } else {
     resamps<- 1:nobjects
   }
   MSY <- output$MSY[resamps,]
   Fmsy <- output$Fmsy[resamps,]
   SSBmsy <- output$SSBmsy[resamps,]
   B10msy <- output$B10msy[resamps,]
   TBmsy <- output$TBmsy[resamps,]
   SSB <- output$SSB[resamps,]
   B10 <- output$B10[resamps,]
   TOTbio <- output$TOTbio[resamps,]
   FFmsy <- output$FFmsy[resamps,]
   UUmsy <- output$UUmsy[resamps,]
   BBmsy <- output$BBmsy[resamps,]
   B10B10msy <- output$B10B10msy[resamps,]
   scenario <- output$scenario[resamps]
   B0 <- output$SSB0[resamps]
   B.B0 <- SSB / B0 # B0 is a vector and SSB a matrix
   B10.B0 <- B10 / B10[,1]

   pquantiles <- c(0.05,0.10,0.25,0.5,0.75,0.9,0.95)
   Bquantiles = round(apply(BBmsy,2,quantile,p=pquantiles,na.rm=T),3)
   B10quantiles = round(apply(B10B10msy,2,quantile,p=pquantiles,na.rm=T),3)
   B.B0quantiles = round(apply(B.B0,2,quantile,p=pquantiles,na.rm=T),3)
   B10.B0quantiles = round(apply(B10.B0,2,quantile,p=pquantiles,na.rm=T),3)
   Uquantiles = round(apply(UUmsy,2,quantile,p=pquantiles,na.rm=T),3)
   Fquantiles = round(apply(FFmsy,2,quantile,p=pquantiles,na.rm=T),3)
   B10msyB0quantiles = round(quantile(B10msy[,nyears-1]/B10[,1],p=pquantiles,na.rm=T),3)
   SSBmsyB0quantiles = round(quantile(SSBmsy[,nyears-1]/B0,p=pquantiles,na.rm=T),3)

 return(list("scenario"=scenario,"MSY"=MSY,"SSBmsy"=SSBmsy,"B10msy"=B10msy,"TBmsy"=TBmsy,"Fmsy"=Fmsy,
   "FFmsy"=FFmsy,"UUmsy"=UUmsy,"SSB"=SSB,"B10plus"=B10,"TOTbio"=TOTbio,"B0"=B0,
   "B.Bmsy.quantiles"=Bquantiles,"B10.B10msy.quantiles"=B10quantiles,
   "B.B0.quantiles"=B.B0quantiles,"B10.B0.quantiles"=B10.B0quantiles,
   "FFmsyquantiles"=Fquantiles,"UUmsyquantiles"=Uquantiles,
   "B10msyB0quantiles"=B10msyB0quantiles, "SSBmsyB0quantiles"= SSBmsyB0quantiles,
   "output"= fits))
}

