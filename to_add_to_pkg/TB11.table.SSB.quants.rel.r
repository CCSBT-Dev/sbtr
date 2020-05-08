#' @title Table SSB quants rel
#' @description
#' Returns a table containing the scenario and the probability that the spawning biomass in yr.required is greater than the spawning biomass in year relative.to.
#' @export
#' 
table.SSB.quants.rel <- function(s4files, relative.to=2004, yr.required=2014, 
              B.option=c("rel.to.single","rel.to.median"),label.names, quants=c(0.1,0.5,0.9), filenames=T)
{
#s4files = vector of .s4 files, including paths
#relative.to = vector of years
#yr.required = vector of years
#quants = vector of quantiles, e.g. c(0.1, 0.5, 0.9)
#B.option = "rel.to.single" is probability of increase for each individual run
#         = "rel.to.median" is probability greater than median in relative.to year. 

 B.option <- B.option[1]
 
  if (filenames==T) {
    nfiles <- length(s4files)  
  }
  else {
    nfiles <- 1
  }

 nquants <- length(quants)
 nyr <- length(yr.required)
 temp <- array(data=0,dim=c(nfiles,nyr),
                dimnames=list(label.names,paste("B",yr.required,"/B",relative.to,sep="")))
 results.list <- list()
 for (k in 1:nquants) {
    results.list[[k]] <- temp
 }
 names(results.list) <- paste("Quant",quants,sep="")
 
 for (i in 1:nfiles) { 
    if (filenames==T) {   #the names of files are passed
      tmp <- read.table(s4files[i],skip=2,header=T)
    }
    else {                #the actual file as passed, only supports ONE file
      tmp <- s4files
    }

    tmp <- as.matrix(tmp[,-c(1,2)])

    for (j in 1:nyr) {
      #find the first year biomass is reported in the file, should be 1931
      col.required <- grep(paste("B.",yr.required[j],sep=""),colnames(tmp))
      col.rel.to <- grep(paste("B.",relative.to[j],sep=""),colnames(tmp))

      if(B.option=="rel.to.single") bworms<- tmp[,col.required]/tmp[,col.rel.to]
      if(B.option=="rel.to.median") bworms<- tmp[,col.required]/quantile(tmp[,col.rel.to],0.5)

      res.quants <- quantile(bworms,probs=quants)
      for (k in 1:nquants) {
          results.list[[k]][i,j] <- res.quants[k]
      }
    }
 }    
 return(results.list)
}
#######Table 4 The 10th, 50th, 90th quantiles of Boneyear/Banotheryear
###source("TB10.table.SSB.quants.rel.r")
#catch.levels <- seq(0,10000,1000)
#s4files <- paste("CONST\\v0\\CONST",catch.levels,"_c1s1l13h.s4",sep="")
#x <- table.SSB.quants.rel(s4files=s4files, label.names=paste(catch.levels,"mt"),
#                        relative.to=c(1980,2004,2009,2009),yr.required=c(2020,2014,2014,2025), 
#                        quants=c(0.1,0.5,0.9),B.option="rel.to.single")
  
