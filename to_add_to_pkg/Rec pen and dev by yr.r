###########################################
#Plot of recruitment deviates and recruitment penalty
#Trevor A. Branch 24 July 2013
#############################################

plot.rec.dev <- function(data.objects) {
   nobjects <- length(data.objects)
   
   nrecs <- length(data.objects[[1]]$Rdev)
   rec.data <- matrix(nrow=nobjects, ncol=nrecs)
   
   for (i in 1:nobjects) {
      rec.data[i,] <-data.objects[[i]]$Rdev   
   }
   
   
   
   invisible(rec.data)
   
}
#temp <- plot.rec.dev(data.objects=data.base)
#par(mfrow=c(2,1), oma=c(3,3,1,1), mar=c(0,0,2,0))
#plot(x=1931:2012, temp[1,], xaxt="n", pch=19,main="Recruitment deviates")
#plot(x=1931:2012, temp[1,]^2/(2*0.6^2), pch=19, main="Recruitment NLL penalty")