resample.idea <- function(data.objects,label,lev.file) {
  nobjects <- length(data.objects)
  MSY.data <- matrix(nrow=nobjects,ncol=9)
  x <- read.csv(file=lev.file,header=F,colClasses="numeric",sep=" ")
  nlevs <- nrow(x)
  lev.scens <- vector(length=nobjects)
  for (i in 1:nlevs) {
     lev.scens[i] <- as.numeric(paste(x[i,1],x[i,2],x[i,3],x[i,4],x[i,5],x[i,6],sep=""))
  }
 
  resamps <- match(lev.scens,MSY.data[,1])
  return(MSY.data[resamps,])
}
