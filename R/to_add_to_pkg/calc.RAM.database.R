#' RAM <- calc.RAM.database.R(base2013,lev.file="base2013sqrt.lev")
#' 
#' calculates time series and reference points for  RMAII legacy database
#' U/Umsy , B/Bmsy B10/B10msy and B/B0 over time with confidence intervals. 
#' Umsy and Bmsy were calculated by msycalc.tpl using anual weights, selectivities and allocation
#' data.objects is the collection of lab.rep files produced by get.all.data.R()
#' NB: there may be false convergences in msycalc which are replaved by NA.
#' to check use:  apply(MSY.stats$output$MSY,2,countNAs)   (countNAs is length(x[is.na(x)])
#' Ana Parma, 2014 
#' 
#' @param y the function input
#' @return something done on y
#' @author Ana Parma
#' @examples
#' RAM <- calc.RAM.database.R(base2013).
#' @export
#'
calc.RAM.database.R<-function(data.objects,lev.file="base.lev")
{
# data.objects is the collection of lab.rep files produced by get.all.files.R()
   xx <- data.objects[[1]]
   years=xx$yrs.catch[1]:(xx$yrs.catch[2]+1) 
   nyears = length(years)
   nobjects <- length(data.objects)
   allyears = xx$years[1]:(xx$years[2]+1)
   nallyears = length(allyears)
   id.years <- allyears %in% years   #for selecting years with catch > 0 and last year + 1

   var.names = c("scenario","SSB","TOTbio","B10","Rec","MSY","TBmsy","SSBmsy","B10msy","Fmsy","F","BBmsy","UUmsy","FFmsy","SSB0")
   output=vector("list",length(var.names))
   names(output)=var.names
   mm <-matrix(nrow=nobjects,ncol=(nyears))  # number of years with catch plus last year +1 
   colnames(mm) <- years
 #  output=vector("list",length(var.names))
   output <- rep(list(mm),length(var.names))   #create a list of matrices
   names(output)=var.names
# replace these two by vectors
   output$SSB0=rep(NA,nobjects)
   output$scenario=rep(NA,nobjects)

   for (i in 1:nobjects)
   {
      xx <- data.objects[[i]]
      output$scenario[i]=xx$scenario_number
      output$SSB[i,]=xx$Sbio[id.years]   #selects up to final year with catch +1
      output$TOTbio[i,]=xx$TOTbio[id.years]
      output$Rec[i,]=xx$Recruitment[id.years]
      output$Fmsy[i,1:(nyears-1)] = xx$Fmsy_a215
      output$Fmsy[i,nyears]= output$Fmsy[i,nyears-1] 
      output$TBmsy[i,1:(nyears-1)]=xx$TBmsy   #total biomass
      output$TBmsy[i,nyears]= output$TBmsy[i,nyears-1] 
      output$SSBmsy[i,1:(nyears-1)]=xx$Bmsy  #spawning stock biomass
      output$SSBmsy[i,nyears]=output$SSBmsy[i,nyears-1]  #set equal to last year of catch
      output$MSY[i,1:(nyears-1)]=xx$MSY
      output$MSY[i,nyears]=output$MSY[i,nyears-1]  #set equal to last year of catch
      output$F[i,1:(nyears-1)] = xx$F_a215
      output$SSB0[i] = xx$B0
      output$B10[i,]=xx$B10_plus[id.years]
      output$B10msy[i,1:(nyears-1)] =  apply(xx$Bmsy_a[,11:31],1,sum)
      output$B10msy[i,nyears]=output$B10msy[i,nyears-1]  #set equal to last year of catch
   }       
      output$BBmsy =  output$SSB/output$SSBmsy
      output$B10B10msy =  output$B10/output$B10msy
      output$TBTBmsy =  output$TOTbio/output$TBmsy
      output$FFmsy =  output$F/output$Fmsy
      output$BB0 =  output$SSB/output$SSB0   #SSB is a matrix and SSB0 a vector
      output$B10B0 =  output$B10/output$B10[,1]   #B10 is a matrix and B10[,1] a vector

   #check convergence and set to NA
   id.wrong <-  output$MSY < 1

   output$SSBmsy[id.wrong]=NA   
   output$TBTBmsy[id.wrong]=NA   
   output$B10msy[id.wrong]=NA   
   output$FFmsy[id.wrong]=NA   
   output$BBmsy[id.wrong]=NA    
   output$B10B10msy[id.wrong]=NA    
   output$MSY[id.wrong]=NA    
   output$Fmsy[id.wrong]=NA   
 

 # sample over grid    
    lev <- read.table(file=lev.file,colClasses="numeric",sep=" ")
    nlevs = nrow(lev)    # 
    lev.scens <- as.numeric(paste(lev[,1],lev[,2],lev[,3],lev[,4],lev[,5],lev[,6],lev[,7],sep="")) 
    resamps <- match(lev.scens,output$scenario)
 
   MSY=apply(output$MSY[resamps,],2,median,na.rm=TRUE)

   R=apply(output$Rec[resamps,],2,median,na.rm=TRUE)
   F=round(apply(output$F[resamps,],2,median,na.rm=TRUE),4)
   SSB=apply(output$SSB[resamps,],2,median,na.rm=TRUE)
   B10=round(apply(output$B10[resamps,],2,median,na.rm=TRUE),0)
   TOTbio=round(apply(output$TOTbio[resamps,],2,median,na.rm=TRUE),0)
   Fmsy=round(apply(output$Fmsy[resamps,],2,median,na.rm=TRUE),4)
   FFmsy=round(apply(output$FFmsy[resamps,],2,median,na.rm=TRUE),3)
   BBmsy=round(apply(output$BBmsy[resamps,],2,median,na.rm=TRUE),3)
   B10B10msy=round(apply(output$B10B10msy[resamps,],2,median,na.rm=TRUE),3)
   TBmsy=apply(output$TBmsy[resamps,],2,median,na.rm=TRUE)
   TBTBmsy=round(apply(output$TBTBmsy[resamps,],2,median,na.rm=TRUE),3)
   SSBmsy=apply(output$SSBmsy[resamps,],2,median,na.rm=TRUE)
   B.B0=round(apply(output$BB0[resamps,],2,median,na.rm=TRUE),3)
   B10.B0=round(apply(output$B10B0[resamps,],2,median,na.rm=TRUE),3)

   B0 = output$SSB0[resamps]

   TS = data.frame("year"=years,"SSB"=SSB,"Recruits"=R,"B10"=B10,"TOTbio"= TOTbio,"F"=F,"Fmsy"=Fmsy, 
                   "SSB.SSBmsy"= BBmsy,"B10.B10msy"=B10B10msy,"TB.TBmsy"=TBTBmsy, "F.Fmsy"= FFmsy,
                   "SSB.SSB0"=B.B0,"B10.B0"=B10.B0)
   stats = list("MSY"=MSY[nyears-1],
                "SSB0"=median(B0),"B10.0"=B10[1],"TOTB0"=TOTbio[1],"Fmsy"=Fmsy[nyears-1],
                "SSBmsy"=SSBmsy[nyears-1],"TBmsy"=TBmsy[nyears] )


return(list("TS"=TS,"stats"=stats))
}
