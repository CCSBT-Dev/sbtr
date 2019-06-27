################################################################################
#Get the residuals in the recruitment and calculate autocorrelation
#Also calculate the penalty in the recruitment.
#For different periods of time 30s/40s, 50s/60s, 70s/80s, 90s/00s, 
#Trevor A. Branch, started 23 July 2013. Portland ME, USA.
#tbranch@gmail.com / tbranch@uw.edu
################################################################################


#####################################################################
#
#####################################################################
Rec.autocorrel.pen<- function(data.objects,label="") {
   nobjects <- length(data.objects)
   rec.data <- matrix(nrow=nobjects,ncol=4)
   pen.data <- matrix(nrow=nobjects,ncol=4)
   colnames(rec.data)=c("30s-40s" ,"50s-60s" ,"70s-80s","90s-00s")
   colnames(pen.data)=c("30s-40s" ,"50s-60s" ,"70s-80s","90s-00s")
   
   for (i in 1:nobjects) {
      x <- data.objects[[i]]
      lenR <- length(x$Rdev)
      rec.data[i,1] <- cor(x$Rdev[2:19],x$Rdev[1:18])
      rec.data[i,2] <- cor(x$Rdev[21:39],x$Rdev[20:38])
      rec.data[i,3] <- cor(x$Rdev[41:59],x$Rdev[40:58])
      rec.data[i,4] <- cor(x$Rdev[61:79],x$Rdev[60:78])

      pen.data[i,1] <- sum(x$Rdev[1:19]^2)/(2*0.6^2)
      pen.data[i,2] <- sum(x$Rdev[20:39]^2)/(2*0.6^2)
      pen.data[i,3] <- sum(x$Rdev[40:59]^2)/(2*0.6^2)
      pen.data[i,4] <- sum(x$Rdev[60:79]^2)/(2*0.6^2)
      
   }
   pen.95 <- round(apply(pen.data,FUN=quantile,c(0.05,0.5,0.95), MARGIN=2),3)
   rec.95 <- round(apply(rec.data,FUN=quantile,c(0.05,0.5,0.95), MARGIN=2),3)
   
   return(list(rec.autocorrel=rec.95, total.penalty=pen.95))
}
#source("get.all.files.r")
#data.base <- get.all.files("arc\\base2010sqrt")
#Rec.autocorrel.pen(data.objects=data.base,label="")