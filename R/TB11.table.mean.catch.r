mean.Catch <- function(s3filenames, start.yr=seq(2013,2028,3), end.yr=seq(2015,2030,3), scen.names)
{
    base.s3.actual.files <- list()
    nfiles <- length(s3filenames)
    nyears <- length(start.yr)
    for (i in 1:nfiles) {
        base.s3.actual.files[[i]] <- read.table(s3filenames[i],skip=2,header=T)
    }
    results <- matrix(nrow=nfiles,ncol=nyears, dimnames=list(scen.names,paste(start.yr,"-",end.yr,sep="")))
    for (i in 1:nfiles) {
        s3file <- base.s3.actual.files[[i]]
        for (j in 1:nyears) {  
            catch.cols <- c(grep(paste("C.",start.yr[j],sep=""),colnames(s3file)),grep(paste("C.",end.yr[j],sep=""),colnames(s3file)))
            catches <- s3file[,catch.cols[1]:catch.cols[2]]
            results[i,j] <- round(median(rowMeans(catches)),0) 	  
        }
    }
    invisible(results)
}
