#########################################################
#Computes and plots SPR
#Ana Sept 2009
##################################################################
plot.SPR <- function(data.objects,lev.file="MpowerLL3sel.lev",msy=F,allSPRmsy=F) 
{
 # require(sfsmisc)
  # data.objects is a list of the _lab.rep files from all grid cells
  # produced with Trevor's get.all.data.r
  # the lev.file has the scenarios of 2000 sampled grid cells

   xx <- data.objects[[1]]
   years=min(xx$Fs[,2]):max(xx$Fs[,2])
   nyears = length(years)
   nobjects <- length(data.objects)
   allyears = xx$years[1]:xx$years[2]
  
   SPR = matrix(NA,nobjects,nyears)  
    SPRmsy = vector(length=nobjects)  
   
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
      
      CumSurv = matrix(1,nyears,31)
      CumSurv[,2:31] = t(apply(Z[,-31],1,cumsum))
      CumSurv = exp(-CumSurv)
      CumSurv[,31] = CumSurv[,31]/(1-Surv[,31])
 
      id = allyears>= min(years)
      fec = xx$spweights.age[id,]
      fec[,1:10]=0
      fec0 = xx$spweights.age[1,]
      fec0[1:10]=0
      CumSurvM = rep(1,31)
      CumSurvM[2:31] = exp(-cumsum(M[-31]))
      CumSurvM[31]=CumSurvM[31]/(1-exp(-M[31]))
      SPR0 = sum(CumSurvM*fec0)
      SPR[i,] = rowSums(CumSurv * fec)/SPR0
      if(msy==T)
      {
       Zmsy = c(0,xx$Fmsy_a)+M
       CumSurvMSY=rep(1,31)
       CumSurvMSY[2:31] = exp(-cumsum(Zmsy[-31]))
       CumSurvMSY[31] = CumSurvMSY[31]/(1-exp(-Zmsy[31]))
       fec2008 = xx$spweights.age[nrow(xx$spweights.age),]
       fec2008[1:10]=0
       SPRmsy[i] = sum(CumSurvMSY*fec2008)/SPR0
      }  
 }
 
    lev <- read.table(file=lev.file,colClasses="numeric",sep=" ")
    nlevs = nrow(lev)
    lev.scens <- vector(length=nobjects)
  for (i in 1:nlevs) 
  {
     lev.scens[i] <- as.numeric(paste(lev[i,1],lev[i,2],lev[i,3],lev[i,4],lev[i,5],lev[i,6],sep=""))
  }
    resamps <- match(lev.scens,scenario)
    result= SPR[resamps,]
  # boxplot.matrix(result,names=years,cex=0.4,col=3,las=1,yaxs="i",ylim=c(0,1))
   boxplot(data.frame(result),names=years,cex=0.4,col=3,las=1,yaxs="i",ylim=c(0,1))
 
  if(msy == T) {
      if (allSPRmsy==F) {
        abline(h=median(SPRmsy),lwd=2,lty=2,col="gray40")
      }
      else {
        abline(h=SPRmsy,lwd=2,lty=2,col="gray40")
      }
      x <- par("usr")[1]+0.8*diff(par("usr")[c(1:2)])
      print(x)
      text(x=x,y=median(SPRmsy)+0.03,"SPR at FMSY",cex=1.5,col="gray40")
  }
  mtext(side=1,line=3,"Year",cex=1.5)
  mtext(side=2,line=3,"Spawning biomass per recruit (relative to unfished)",cex=1.5)

  invisible(SPRmsy)
}
