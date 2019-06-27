######################################################################
#Given the output files for CCSBT model sbtprojv117.tpl, calculates various
#statistics of interest and produces a .csv file
#Main statistics are SSB(year)/SSB(some other year) and 
#probability of SSB(year)/SSB(some other year) being greater than one. 
#Written by Trevor A Branch 4 July 2009, tbranch@gmail.com
######################################################################


###################prob.B.gt#########################################
#Given two years, an s4 file, and the start yr of the model, returns the probability 
#that the spawning biomass in the later year is greater than the spawning biomass 
#in the earlier year.
#Trevor A. Branch tbranch@gmail.com
#####################################################################
prob.B.gt <- function(s4file = "CONSTmod21\\current\\C1S1L1mod_current.s4", start.yr=1931, yr.required=2014, 
                      relative.to=2004)  {
    SSB.data <- read.table(s4file,skip=3)
    SSB.data <- as.matrix(SSB.data[,-c(1,2)])
    
    B.relto.yr <- SSB.data[,relative.to - start.yr+1]
    B.required.yr <- SSB.data[,yr.required - start.yr+1]
    bworms <- B.required.yr/B.relto.yr
    calc.prob <- sum(bworms>=1)/NROW(bworms)
    return(calc.prob)
}
#prob.B.gt(s4file = "CONSTmod21\\current\\C1S1L1mod_current.s4", start.yr=1931, yr.required=2014, relative.to=2004)

####Example of using this in a loop to generate a table (of course the dirs and names will vary)
#models <- c("CONSTmod21","CONSTmod22")
#dirs <- c("zero","down4000","down2000","current","up2000","up4000")
#filepref <- c("C1S1L1mod_","C1S1L1_")
#probs <- c("p(B2014>B2004)","p(B2022>B2004)","p(B2014>B2009)","p(B2022>B2009)")
#rowsname <- c(paste("mod21",probs),paste("mod22",probs))
#result <- matrix(ncol=length(dirs),nrow=length(models)*4,dimnames=list(rowsname,dirs))
#for (mm in 1:length(models)) {
#   for (dd in 1:length(dirs)) {
#      s4file <- paste(models[mm],"\\",dirs[dd],"\\",filepref[mm],dirs[dd],".s4",sep="")
#      result[mm,dd] <- prob.B.gt(s4file=s4file, start.yr=1931, yr.required=2014, relative.to=2004)
#      result[2+mm,dd] <- prob.B.gt(s4file=s4file, start.yr=1931, yr.required=2022, relative.to=2004)
#      result[4+mm,dd] <- prob.B.gt(s4file=s4file, start.yr=1931, yr.required=2014, relative.to=2009)
#      result[6+mm,dd] <- prob.B.gt(s4file=s4file, start.yr=1931, yr.required=2022, relative.to=2009)
#   }
#}


###################prob.B.gt#########################################
#Given two years, an s4 file, and the start yr of the model, returns the probability 
#that the spawning biomass in the later year is greater than the spawning biomass 
#in the earlier year.
#Trevor A. Branch tbranch@gmail.com
#####################################################################
B.ratio.quants <- function(s4file = "CONSTmod21\\current\\C1S1L1mod_current.s4", start.yr=1931, yr.required=2014, 
                      relative.to=2004, quants=c(0.1,0.3,0.5))  {
    SSB.data <- read.table(s4file,skip=3)
    SSB.data <- as.matrix(SSB.data[,-c(1,2)])
    
    B.relto.yr <- SSB.data[,relative.to - start.yr+1]
    B.required.yr <- SSB.data[,yr.required - start.yr+1]
    bworms <- B.required.yr/B.relto.yr
    B.ratios <- quantile(bworms,quants)
    return(B.ratios)
}
#x <- B.ratio.quants(s4file = "C1S1L1m4bounded.s4", start.yr=1931, yr.required=2020, relative.to=2004, quants=c(0.05,0.5,0.95))
#round(x,3)
####Example of using this in a loop to generate a table (of course the dirs and names will vary)
#models <- c("CONSTmod21","CONSTmod22")
#dirs <- c("zero","down4000","down2000","current","up2000","up4000")
#filepref <- c("C1S1L1mod_","C1S1L1_")
#quants <- c(0.1,0.3,0.5,0.7,0.9)
#probs <- c("B2014/B2004","B2009/B0","B2014/B2004","B2022/B2004","B2014/B2009","B2022/B2009")
#yr.required <- c(2014,2009,2014,2022,2014,2022)
#relative.to <- c(2004,1931,2004,2004,2009,2009)
#result <- matrix(ncol=length(quants)*length(models)+2,nrow=length(dirs)*length(probs))
#colnames.temp <- c("Ref point","Catches")
#temp <- c("21","22")
#for (i in 1:length(models)) {
#   colnames.temp <- c(colnames.temp,paste(temp[i],quants))
#}
#colnames(result) <- colnames.temp
#rownum <- 1
#for (pp in 1:length(probs)) {
#  for (dd in 1:length(dirs)) {
#    result[rownum,1] <- probs[pp]
#    result[rownum,2] <- dirs[dd]
#    for (mm in 1:length(models)) {
#      s4file <- paste(models[mm],"\\",dirs[dd],"\\",filepref[mm],dirs[dd],".s4",sep="")
#      result[rownum,(3+length(quants)*(mm-1)):(3+length(quants)*mm-1)] <- 
#                  round(B.ratio.quants(s4file=s4file, start.yr=1931, yr.required=yr.required[pp], relative.to=relative.to[pp], quants=quants),4)
#    }
#      rownum <- rownum+1
#  }
#}
#result
