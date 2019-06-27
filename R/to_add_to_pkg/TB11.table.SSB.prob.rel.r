#' @title Table SSB prob rel
#' @description
#' Returns a table containing the scenario and the probability that the spawning biomass in yr.required is greater than the spawning biomass in year relative.to.
#' @export
#' 
table.SSB.prob.rel <- function(s4files, relative.to=c(2004), yr.required=c(2014), 
              B.option=c("rel.to.single","rel.to.median"),label.names)
{
#s4files = vector of .s4 files, including paths
#relative.to = vector of years
#yr.required = vector of years
#B.option = "rel.to.single" is probability of increase for each individual run
#         = "rel.to.median" is probability greater than median in relative.to year. 

 B.option <- B.option[1]
 nfiles <- length(s4files)  
 nyrs <- length(yr.required)
 prob.gt <- array(data=0,dim=c(nfiles,nyrs),
                dimnames=list(label.names,paste("P(SSB",yr.required,">SSB",relative.to,")",sep="")))

 for (i in 1:nfiles) { 
    tmp <- read.table(s4files[i],skip=2,header=T)
    tmp <- as.matrix(tmp[,-c(1,2)])

    for (j in 1:nyrs) {
       #find the first year biomass is reported in the file, should be 1931
       col.required <- grep(paste("B.",yr.required[j],sep=""),colnames(tmp))
       col.rel.to <- grep(paste("B.",relative.to[j],sep=""),colnames(tmp))

       if(B.option=="rel.to.single") bworms<- tmp[,col.required]/tmp[,col.rel.to]
       if(B.option=="rel.to.median") bworms<- tmp[,col.required]/quantile(tmp[,col.rel.to],0.5)

       prob.gt[i,j] <- sum(bworms>=1)/NROW(bworms)
     }
 }    
 return(prob.gt)
}
####Table 1 probability that SSB in one year exceeds that in another year, e.g. P(B2014>B2004) 
#source("TB10.table.SSB.prob.rel.r")
#catch.levels <- seq(0,10000,1000)
#s4files <- paste("CONST\\v0\\CONST",catch.levels,"_c1s1l13h.s4",sep="")
#table.SSB.prob.rel(s4files=s4files, label.names=paste(catch.levels,"mt"),
#                        relative.to=c(1980,2004,2009,2009),yr.required=c(2020,2014,2014,2025),B.option="rel.to.single")
  
