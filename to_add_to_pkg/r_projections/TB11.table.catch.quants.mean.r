table.catch.quants.mean <- function(s3files, yr.start=2009, yr.end=2018, 
              label.names, quants=c(0.1,0.5,0.9), filenames=T)
{
#TA Branch: 25 Jan 2010 (multiply revised)
#Returns a table containing the scenario and the probability that the spawning biomass
#in yr.required is greater than the spawning biomass in year relative.to.
#s3files = vector of .s3 files, including paths
#yr.start = vector of years
#yr.end = vector of years
#quants = vector of quantiles, e.g. c(0.1, 0.5, 0.9)
#if filename==T then s4files contains names of files to be read in
#if filename==F then s4files contains list of object of read in files using  x <- read.table(s4files[i],skip=2,header=T)

 if (filenames==T) {
   nfiles <- length(s3files)  
 }
 else {
   nfiles <- 1
 }

 nquants <- length(quants)
 nyr <- length(yr.start)
 temp <- array(data=0,dim=c(nfiles,nyr),
                dimnames=list(label.names,paste("C(",yr.start,"-",yr.end,")",sep="")))
 results.list <- list()
 for (k in 1:nquants) {
    results.list[[k]] <- temp
 }
 names(results.list) <- paste("Quant",quants,sep="")
 
 for (i in 1:nfiles) { 
    if (filenames==T) {   #the names of files are passed
      tmp <- read.table(s3files[i],skip=2,header=T)
    }
    else {                #the actual file as passed, only supports ONE file
      tmp <- s3files
    }

    tmp <- as.matrix(tmp[,-c(1,2)])

    for (j in 1:nyr) {
      #find the first year biomass is reported in the file, should be 1931
      col.start <- grep(paste("C.",yr.start[j],sep=""),colnames(tmp))
      col.end <- grep(paste("C.",yr.end[j],sep=""),colnames(tmp))

      bworms<- rowMeans(tmp[,col.start:col.end])

      res.quants <- quantile(bworms,probs=quants)
      for (k in 1:nquants) {
          results.list[[k]][i,j] <- res.quants[k]
      }
    }
 }    
 return(results.list)
}
#######Table 7 The 10th, 50th, 90th quantiles of mean(catchyr:catchanotheryr)
#Of course only works for 2009 onwards.
#source("TB10.table.catch.quants.mean.r")
#catch.levels <- seq(0,10000,1000)
#s3files <- paste("CONST\\v0\\CONST",catch.levels,"_c1s1l13h.s3",sep="")
#x <- table.catch.quants.mean(s3files=s3files, label.names=paste(catch.levels,"mt"),
#                        yr.start=c(2012,2012,2012),yr.end=c(2021,2026,2031), 
#                        quants=c(0.1,0.5,0.9))
#write.csv(x,"Tables\\TableCatchQuantsMean.csv")
