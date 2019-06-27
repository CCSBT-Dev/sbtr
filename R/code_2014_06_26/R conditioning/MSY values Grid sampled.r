################################################################################
#Get the MSY results from each cell of the grid to see what the distribution
#of values is, THEN resample them in the same distribution of the grid samples.
#These are taken from the lev files. 
#
#Trevor A. Branch, started 5 September 2009, Busan, South Korea.
################################################################################

#################################################
#Goes to the specified directory and finds all the _lab.rep files.
#Loads them one by one into a massive R list of objects, which takes time.
#Returns the list. This ensures that the reading in stage (which can take 3-10 minutes)
#need only be done once, and then subsequent function calls looping through 
#an entire grid will run in a few seconds instead of minutes. 
#Trevor A. Branch (assistance from Ana Parma) July 2009 
#################################################
get.all.files <- function(directory) {
    library(PBSmodelling)
    files<-dir(directory,pattern="lab.rep")
    nn = length(files)

    result <- list()
    for (i in 1:nn) {
       print(paste(i,"of",nn))
       result[[i]] <- readList(paste(directory,"\\",files[i],sep=""))     
       names(result)[i] <- files[i]
    }
    
    return(result)
}
#directory <- "C:\\Documents and Settings\\Trevor B\\My Documents\\CCSBT\\sbtmod22\\arc\\c1s1l1sqrt"
#data <- get.all.files(directory=directory)


#####################################################################
#plot histograms of MSY values
#Trevor A. Branch 
#####################################################################
MSY.vals.grid <- function(data.objects,label,lev.file) {
  nobjects <- length(data.objects)
  MSY.data <- matrix(nrow=nobjects,ncol=9)
  x <- read.csv(file=lev.file,header=F,colClasses="numeric",sep=" ")
  nlevs <- nrow(x)
  lev.scens <- vector(length=nlevs)
  for (i in 1:nlevs) {
     lev.scens[i] <- as.numeric(paste(x[i,1],x[i,2],x[i,3],x[i,4],x[i,5],x[i,6],sep=""))
  }
  
  for (i in 1:nobjects) {
    MSY.data[i,1] <- data.objects[[i]]$scenario_number
    MSY.data[i,2] <- data.objects[[i]]$MSY
    MSY.data[i,3] <- data.objects[[i]]$Bmsy
    MSY.data[i,4] <- data.objects[[i]]$TBmsy
    MSY.data[i,5] <- data.objects[[i]]$Fmsy_a215
    MSY.data[i,6] <- data.objects[[i]]$F_a215
    temp <- data.objects[[i]]$Sbio
    MSY.data[i,7] <- temp[length(temp)]   #current spawning biomass
    MSY.data[i,8] <- MSY.data[i,6]/MSY.data[i,5]
    MSY.data[i,9] <- MSY.data[i,7]/MSY.data[i,3]
  }
  resamps <- match(lev.scens,MSY.data[,1])
  
  colnames(MSY.data)=c("Scenario" ,"MSY" ,"SBmsy","TBmsy","Fmsy","F", "SBcurrent","F/Fmsy","SB/SBmsy")
    
  par(mfrow=c(4,2),mar=c(3,3,1,0),oma=c(1,2,4,1),mgp=c(1,1,0)) 

  for (i in 2:9)
  {
     yy = MSY.data[resamps,i]
     nameplot = colnames(MSY.data)[i]
     hist(yy,main=nameplot,xlab="",ylab="",col="gray80",xaxs="i",yaxs="i",axes=F,las=1)
     axis(1,pos=0)
     axis(2,line=0,las=1)
  }
  mtext(paste("MSY information",label),side=3,line=1,cex=1.2,outer=T)
  mtext("Frequency",side=2,line=0,cex=1.2,outer=T)
  return(MSY.data[resamps,])
}
#directory <- "\\arc\\c1s1l1sqrt"
#data <- get.all.files(directory=directory)
#pdf(file="figs\\MSY.pdf",width=10,height=8)
#win.graph(width=10,height=8)
#x <- MSY.vals.grid(data.objects=data.MpowerLL3,label="Mpower LL3",lev.file="MpowerLL3\\arc\\c1s1l1\\c1s1l1sqrt.lev")
#dev.off()
#####source("MSY values Grid sampled.r")
