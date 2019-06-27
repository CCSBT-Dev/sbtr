#########################################################
#Boxplots Harvest rates by age ranges 
#Ana Sept 2009
##################################################################
plot.Hs <- function(data.objects,lev.file="MpowerLL3sel.lev") 
{
  # data.objects is a list of the _lab.rep files from all grid cells
  # produced with Trevor's get.all.data.r
  # the lev.file has the scenarios of 2000 sampled grid cells
   xx <- data.objects[[1]]
   years=min(xx$Fs[,2]):max(xx$Fs[,2])
   nyears = length(years)
   nobjects <- length(data.objects)
   
  
    H.ages2.4 = matrix(NA,nobjects,nyears)  
    H.ages5.10 = matrix(NA,nobjects,nyears)  
    H.ages11plus = matrix(NA,nobjects,nyears)  
    scenario = vector(length=nobjects)
   for (i in 1:nobjects)
   {
      xx <- data.objects[[i]]
      fishery = xx$Fs[,1]
      scenario[i] = xx$scenario_number
   # harvest rate by season
      H1 = xx$Fs[fishery ==3,4:33]+xx$Fs[fishery==4,4:33]+xx$Fs[fishery==5,4:33]+xx$Fs[fishery==6,4:33]
      H2 = xx$Fs[fishery ==1,4:33]+xx$Fs[fishery==2,4:33]
      H = 1-(1-H1)*(1-H2)
      H.ages2.4[i,] = round(rowMeans(H[,2:4]),3)    
      H.ages5.10[i,] = round(rowMeans(H[,5:10]),3)    
      H.ages11plus[i,] = round(rowMeans(H[,11:30]),3)   
   }
   
   
    lev <- read.table(file=lev.file,colClasses="numeric",sep=" ")
    nlevs = nrow(lev)
    lev.scens <- vector(length=nobjects)
  for (i in 1:nlevs) 
  {
     lev.scens[i] <- as.numeric(paste(lev[i,1],lev[i,2],lev[i,3],lev[i,4],lev[i,5],lev[i,6],sep=""))
  }
    resamps <- match(lev.scens,scenario)
  result= list("scenario"=lev.scens,"years"=years,"Hages2.4"=H.ages2.4[resamps,],"Hages5.10"=H.ages5.10[resamps,],"Hages11plus"=H.ages11plus[resamps,])

  par(mfrow=c(3,1),mar=c(1,1,1,1),oma=c(6,6,0,0))
  boxplot(data.frame(result$Hages2.4),names=result$years,axes=F,yaxs="i",col="green")
  axis(2,las=1)
  box()
  par(new=T, usr=c(0, 1, 0, 1))
  text(0.08,0.9,"Ages 2-4",cex=1.4)
  boxplot(data.frame(result$Hages5.10),names=result$years,axes=F,yaxs="i",col="green")
  axis(2,las=1)
  box()
  par(new=T, usr=c(0, 1, 0, 1)) 
  text(0.08,0.9,"Ages 5-10",cex=1.4)
  boxplot(data.frame(result$Hages11plus),names=result$years,las=1,yaxs="i",col="green")
  par(new=T, usr=c(0, 1, 0, 1))
  text(0.08,0.9,"Ages 11+",cex=1.4)

  mtext(side=1,outer=T,line=3,"Year",cex=1.6)
  mtext(side=2,outer=T,line=3,"Harvest rates",cex=1.6)
  invisible(result)
}
