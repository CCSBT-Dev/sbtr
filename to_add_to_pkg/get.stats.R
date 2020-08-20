#########################################################
#Extracts a series of statistics from lab_rep files and samples according to the *.lev file
#Ana Sept 2009
##################################################################
get.stats <- function(data.objects,lev.file="base.lev") 
{
 # data.objects is a list of lab_rep files
   nobjects <- length(data.objects)
   depletion = rep(NA,nobjects)
   objF = rep(NA,nobjects)
   R0 = rep(NA,nobjects)
   B0 = rep(NA,nobjects)
   SB = rep(NA,nobjects)
    F215= rep(NA,nobjects)
    Fmsy215= rep(NA,nobjects)
   MSY= rep(NA,nobjects)

    h= rep(NA,nobjects)
   M0= rep(NA,nobjects)
   M10= rep(NA,nobjects)
   M4= rep(NA,nobjects)
   Bmsy= rep(NA,nobjects)
   h= rep(NA,nobjects)   
   scenario = vector(length=nobjects)
  for (i in 1:nobjects)
   {
      xx <- data.objects[[i]]
      scenario[i] = xx$scenario_number
      objF[i] <- sum(xx$lnlike)
      depletion[i] <- round(xx$depletion[1],3)
      R0[i] <- xx$R0[1]
      B0[i] <- xx$B0
      M0[i] <- xx$M[1]
      M4[i] <- round(xx$M[5],3)
      M10[i] <- xx$M[11]
      h[i] <- xx$steep
      SB[i] <- round(xx$Sbio[length(xx$Sbio)],0)
      F215[i] <- round(xx$F_a215,3)
      Fmsy215[i] <- round(xx$Fmsy_a215,3)
      Bmsy[i] = xx$Bmsy
      MSY[i] = xx$MSY
   }
   result<- as.data.frame(cbind(scenario,h,M0,M4,M10,objF,R0,B0,SB,depletion,F215,Fmsy215,Bmsy,MSY))
#   rownames(result) <- names(data.objects)
   colnames(result)<- c("scenario","h","M0","M4","M10","sumNLL","R0","B0","SB","depletion", 
                        "Fa215","Fmsya215","Bmsy" ,"MSY")
      lev <- read.table(file=lev.file,colClasses="numeric",sep=" ")
    nlevs = nrow(lev)
    lev.scens <- vector(length=nobjects)
  for (i in 1:nlevs) 
  {
     lev.scens[i] <- as.numeric(paste(lev[i,1],lev[i,2],lev[i,3],lev[i,4],lev[i,5],lev[i,6],sep=""))
  }
    resamps <- match(lev.scens,scenario)

   return(result[resamps,])
}
