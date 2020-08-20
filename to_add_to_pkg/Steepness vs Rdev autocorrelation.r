#what is autocorrelation?
#install.packages("PBSmodelling")
require(PBSmodelling)

####################################################################
#Read in all data from the .lab rep files. 
source("get.all.files.r")
#This takes a long time (10-15 minutes)
#data.base <- get.all.files("arc\\base2013sqrt_UA")

nobj <- length(data.base)
rec.AC <- vector(length=nobj)
steepness <- vector(length=nobj)
for (i in 1:nobj) {
   rec.AC[i] <- data.base[[i]]$AC_penalty[2]
   steepness[i] <- data.base[[i]]$steep
   
}
hist(rec.AC)
plot(x=steepness, y=rec.AC)

   