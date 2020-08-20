#####################################################
#Boxplots for spawning biomass age 10+
#Note: these are not revisited from the 2000 grid, these are the 
#values from the 320 scenarios
#There will be  switch to go from one to the other using the .lev
#file
#####################################################

plot.SSB10plus.boxplots.r <- function(objects, label="", type=c("obj","grid")) {
   #type="obj" just do this for the 320 cells with no resampling
   #type="grid" resample from the grid to get the 2000 cells
   
   type <- type[1]  
   par(mfrow=c(1,1),oma=c(0,0,0,0),mar=c(5,5,3,1))
   nobjects <- length(objects) 
   
   nssb <- length(objects[[1]]$B10_plus)
   #print(nssb)
   years <- c(objects[[1]]$years[1]:(objects[[1]]$years[2]+1))
   #print(length(years))
   depl <- matrix(nrow=nobjects, ncol=nssb)
   for (i in 1:nobjects) {
      x <- objects[[i]]
      depl[i,] <- x$B10_plus/x$B10_plus[1]   #since first element is SSB0
   }
   quants <- apply(depl,MARGIN=2,quantile,c(0.05,0.5,0.95))
   #print(dim(quants))
   plot(x=years,y=quants[2,],type="l", ylim=c(0,1.1*max(quants)), col="black",
        lwd=2,lty=1, yaxs="i", xaxs="i", las=1, ylab="Total B 10+ / unexpl", xlab="Years")
   lines(x=years,y=quants[1,],ylim=c(0,1.2*max(quants)), col="black",
        lwd=1,lty=2)
   lines(x=years,y=quants[3,],ylim=c(0,1.2*max(quants)), col="black",
         lwd=1,lty=2)
   mtext(side=3,line=1,label,cex=1.2, outer=F)
   
   
   invisible(quants)
}
#source("get.all.files.r")
#data.base.NS <- get.all.files("arc\\base2010sqrtns")
#temp <- plot.SSB10plus.boxplots.r(objects=data.base.NS, label="New selectivity")
#names(data.base.NS[[1]])
#data.base.NS[[1]]$years
