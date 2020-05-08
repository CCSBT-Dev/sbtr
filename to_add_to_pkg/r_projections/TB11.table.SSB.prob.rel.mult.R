table.SSB.prob.rel.mult <- function(s4files, relative.to=c(2004), yr.required=c(2014),mult=1, 
              B.option=c("rel.to.single","rel.to.median"),label.names, filenames=T)
{
#TA Branch: 25 Jan 2010 (multiply revised)
#Returns a table containing the scenario and the probability that the spawning biomass
#in yr.required is greater than the spawning biomass in year relative.to.
#s4files = vector of .s4 files, including paths
#relative.to = vector of years
#yr.required = vector of years
#mult = value to multiply relative.to biomass by, e.g. for P(B2022> 0.1*B1980), mult=0.1,relative.to=1980
#B.option = "rel.to.single" is probability of increase for each individual run
#         = "rel.to.median" is probability greater than median in relative.to year.
#if filename==T then s4files contains names of files to be read in
#if filename==F then s4files contains list of object of read in files using  x <- read.table(s4files[i],skip=2,header=T)

 B.option <- B.option[1]
 if (filenames==T) {
   nfiles <- length(s4files)  
 }
 else {
   nfiles <- 1
 }
 nyrs <- length(yr.required)
 prob.gt <- array(data=0,dim=c(nfiles,nyrs),
                dimnames=list(label.names,paste("P(SSB",yr.required,">",mult,"*SSB",relative.to,")",sep="")))

 for (i in 1:nfiles) { 
    if (filenames==T) {   #the names of files are passed
      tmp <- read.table(s4files[i],skip=2,header=T)
    }
    else {                #the actual file as passed, only supports ONE file
      tmp <- s4files
    }
    tmp <- as.matrix(tmp[,-c(1,2)])

    for (j in 1:nyrs) {
       #find the first year biomass is reported in the file, should be 1931
       col.required <- grep(paste("B.",yr.required[j],sep=""),colnames(tmp))
       col.rel.to <- grep(paste("B.",relative.to[j],sep=""),colnames(tmp))

       if(B.option=="rel.to.single") bworms<- tmp[,col.required]/(mult*tmp[,col.rel.to])
       if(B.option=="rel.to.median") bworms<- tmp[,col.required]/(mult*quantile(tmp[,col.rel.to],0.5))

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
  