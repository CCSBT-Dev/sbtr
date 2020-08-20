##############################################################################
#Attempt to go through all of the grid values to see what the tau_aerial is
#for different runs. 
#
#
#
#Written by Trevor A. Branch 13 July 2009
##############################################################################

#######################################################
#Extract the tau_aerial value from a single par file
#######################################################
get.tau.aerial <- function(parfile = "sbtmod22\\arc\\C1S1L1orig.5\\C1S1L1orig.5_h1m1M1O1C2a1.par") {
   x<-readLines(con=parfile)
   for (i in 1:length(x)) {
      if(length(grep("tau_aerial",x[i]))==1) {
         tau <- as.numeric(x[i+1])
      }
   }
   return(tau)   
}
#tau<-get.tau.aerial(parfile = "sbtmod22\\arc\\C1S1L1orig.5\\C1S1L1orig.5_h1m1M1O1C2a1.par")


#######################################################
#Go through a half-grid and extract all of the aerial 
#tau values.
#Ultimately it would be a good idea to get these values 
#straight from the C1S1L1.dat or similar file.
#######################################################
get.grid.taus <- function(dir="sbtmod22\\arc\\C1S1L1orig.5\\",prefix="C1S1L1orig.5_") {
   counter <- 1
   tau.results <- NULL
   for (h in c(1,2,3)) {
     for (m in c(1,2,3)) {
       for (M in c(1,2,3)) {
         for (O in c(1,2)) {
           for (C in c(2,3)) {
             for (a in c(1,2)) {
                suffix <- paste("h",h,"m",m,"M",M,"O",O,"C",C,"a",a,sep="")
                parfile <- paste(dir,prefix,suffix,".par",sep="")
                tau<-get.tau.aerial(parfile=parfile)
                tau.results <- c(tau.results,tau)
             }
           }
         }
       }
     }
   }
   return(tau.results)
}
#par(mfrow=c(2,1))
#taus.orig.5 <- get.grid.taus(dir="m4est\\arc\\C1S1L1orig.5\\",prefix="C1S1L1orig.5_")
#hist(taus.orig.5)
#taus.sqrt <- get.grid.taus(dir="m4est\\arc\\C1S1L1sqrt\\",prefix="C1S1L1sqrt_")
#hist(taus.sqrt)


#######################################################
#Extract the tau_aerial value from a single par file
#######################################################
get.cpue.sigma <- function(parfile = "sbtmod22\\arc\\C1S1L1orig.5\\C1S1L1orig.5_h1m1M1O1C2a1.par") {
   x<-readLines(con=parfile)
   for (i in 1:length(x)) {
      if(length(grep("tau_aerial",x[i]))==1) {
         tau <- as.numeric(x[i+1])
      }
   }
   return(tau)   
}


get.cpue.sigma <- function(data.objects) {
   nobjects <- length(data.objects)
   tau <- vector(length=nobjects)
   for (i in 1:nobjects) {
      x <- data.objects[[i]]
      tau[i] <- x$tau
   }
   return(tau)   
}
get.cpue.sigma(data.objects=data.example.sqrt)

#######################################################
#Go through a half-grid and extract all of the aerial 
#tau values.
#Ultimately it would be a good idea to get these values 
#straight from the C1S1L1.dat or similar file.
#######################################################
get.cpue.sigma <- function(dir="sbtmod22\\arc\\C1S1L1orig.5\\",prefix="C1S1L1orig.5_") {
   counter <- 1
   tau.results <- NULL
   for (h in c(1,2,3)) {
     for (m in c(1,2,3)) {
       for (M in c(1,2,3)) {
         for (O in c(1,2)) {
           for (C in c(2,3)) {
             for (a in c(1,2)) {
                suffix <- paste("h",h,"m",m,"M",M,"O",O,"C",C,"a",a,sep="")
                parfile <- paste(dir,prefix,suffix,".par",sep="")
                x <- 
                tau<-get.tau.aerial(parfile=parfile)
                tau.results <- c(tau.results,tau)
             }
           }
         }
       }
     }
   }
   return(tau.results)
}
#taus.orig.5 <- get.cpue.sigma(dir="sbtmod22\\arc\\C1S1L1orig.5\\",prefix="C1S1L1orig.5_")
#taus.sqrt <- get.grid.taus(dir="m4est\\arc\\C1S1L1orig.5\\",prefix="C1S1L1orig.5_")
