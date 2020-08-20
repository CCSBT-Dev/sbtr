source("get.all.files.r")

lev <- read.table(file="arc\\grids\\base.lev",colClasses="numeric",sep=" ")
nlevs = nrow(lev)    # 
lev.scens <- as.numeric(paste(lev[,1],lev[,2],lev[,3],lev[,4],lev[,5],lev[,6],sep=""))
 
    #resamps <- match(lev.scens,scenario)
    #result= SPR[resamps,]

###base <- get.all.files(directory="arc\\base2flexLL3")    #320 grid cells

nfiles <- length(base)
nbiomass <- length(base[[1]]$TOTbio)

totalB.orig <- matrix(nrow=nfiles,ncol=nbiomass)
scenarios <- vector(length=nfiles)
for (i in 1:length(base)) {
    totalB.orig[i,] <- base[[i]]$TOTbio
    scenarios[i] <- base[[i]]$scenario_number
}
resamps <- match(lev.scens, scenarios)
totalB.resamp <- totalB.orig[resamps,]

totalB.median <- apply(totalB.resamp,MARGIN=2,median)
write.csv(cbind(1931:2011,totalB.median),file="tables\\SBT totalB.csv")