#' Computes and plots SPR
#' 
#' @param data.objects a list of the _lab.rep files from all grid cells produced with get.all.data.r
#' @param lev.file has the scenarios of 2000 sampled grid cells
#' @author Ana Parma
#' @examples
#' SPR <- Plot.SPR(base25, msy = TRUE)
#' @export
#' 
Plot.SPR <- function(data.objects, lev.file = "base.lev", msy = TRUE, allSPRmsy = FALSE)
{
 # require(sfsmisc)
   xx <- data.objects[[1]]
   years=xx$yrs.catch[1]:xx$yrs.catch[2]
   nyears = length(years)
   nobjects <- length(data.objects)
   allyears = xx$years[1]:xx$years[2]
  
   SPR = matrix(NA,nobjects,nyears)  
   SPRmsy = matrix(NA,nobjects,nyears)  
   SPR0 = vector(length=nobjects)  
 
    scenario = vector(length=nobjects)
   for (i in 1:nobjects)
   {
      xx <- data.objects[[i]]
      fishery = xx$Fs[,1]
      scenario[i] = xx$scenario_number
   # harvest rate by season
      H1 = xx$Fs[fishery ==3,3:33]+xx$Fs[fishery==4,3:33]+xx$Fs[fishery==5,3:33]+xx$Fs[fishery==6,3:33]
      H2 = xx$Fs[fishery ==1,3:33]+xx$Fs[fishery==2,3:33]
      M = xx$M
      S = matrix(exp(-M),nyears,31,byrow=T)
      Surv = (1-H1)*(1-H2)*S
      Z = -log(Surv)    #instantaneous
      
      CumSurv = matrix(0,nyears,31)
      CumSurv[,2:31] = t(apply(Z[,-31],1,cumsum))
      CumSurv = exp(-CumSurv)
      CumSurv[,31] = CumSurv[,31]/(1-Surv[,31])
 
      id = allyears >= xx$yrs.catch[1]
      fec = xx$spweights.age[id,]
      fec[,1:10]=0
      fec0 = xx$spweights.age[1,]   # unexploited
      fec0[1:10]=0
      CumSurvM = rep(1,31)
      CumSurvM[2:31] = exp(-cumsum(M[-31]))
      CumSurvM[31]=CumSurvM[31]/(1-exp(-M[31]))
      SPR0[i] = sum(CumSurvM*fec0)
      SPR[i,] = rowSums(CumSurv * fec)/SPR0[i]
      if(msy==T)
      {
       Zmsy = xx$Fmsy_a+matrix(M,nyears,31,byrow=T)     # this is a matrix nyears x ages
       CumSurvMSY=matrix(0,nyears,31)
       CumSurvMSY[,2:31] = t(apply(Zmsy[,-31],1,cumsum))
       CumSurvMSY = exp(-CumSurvMSY)
       CumSurvMSY[,31] = CumSurvMSY[,31]/(1-exp(-Zmsy[,31]))
       SPRmsy[i,] = rowSums(CumSurvMSY*fec)/SPR0[i]
       id.wrong <-  xx$MSY < 10
       SPRmsy[i,id.wrong]=NA
      }  
   }
       
    lev <- read.table(file=lev.file,colClasses="numeric",sep=" ")
    nlevs = nrow(lev)    # 
    lev.scens <- as.numeric(paste(lev[,1],lev[,2],lev[,3],lev[,4],lev[,5],lev[,6],sep=""))
 
    resamps <- match(lev.scens,scenario)
    result= SPR[resamps,]
    boxplot(data.frame(result),names=years,cex=0.4,col=3,las=1,yaxs="i",ylim=c(0,1))
 
    if(msy == T) 
    {
      if (allSPRmsy==F) 
      {
        medSPRmsy = median(SPRmsy[resamps,nyears],na.rm=T)
        abline(h=medSPRmsy,lwd=2,lty=2,col="gray40")
      }
      else 
      {
        abline(h=SPRmsy,lwd=2,lty=2,col="gray40")
      }
      x <- par("usr")[1]+0.8*diff(par("usr")[c(1:2)])
  #    print(x)
      text(x=x,y=medSPRmsy+0.03,"SPR at FMSY",cex=1.5,col="gray40")
    }
  mtext(side=1,line=3,"Year",cex=1.5)
  mtext(side=2,line=3,"SSB per recruit (relative to unfished)",cex=1.5)
  if(msy==T)
  { 
    #windows()
    boxplot(SPRmsy[resamps,],names=years,cex=0.4,col=3,main="SPRmsy/SPR0")
  }

  return(list("SPR"=result,"SPRmsy"=SPRmsy[resamps,],"SPR0"=SPR0[resamps]))
}
