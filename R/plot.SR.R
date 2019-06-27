##################################################################
# plot.SR.R(base)

plot.SR.R <- function(data.objects) {
   nobjects <- length(data.objects)
   recdevs <- NULL
  
   steep <- NULL
   M1<- NULL
   M10 <- NULL
   SSB <- NULL
   R <- NULL
   alpha <-NULL
   beta <- NULL
   sigmaR <- NULL
   xx<- data.objects[[1]]
   n<- length(xx$Recruitment)
   for (i in 1:nobjects)
   {
      xx <- data.objects[[i]]
      recdevs <- rbind(recdevs,xx$Rdev)
  
      steep <- rbind(steep,xx$steep)
      M1<- rbind(M1,xx$M[2])
      M10 <- rbind(M10,xx$M[11])
      R <- rbind(R,xx$Recruitment[-n])      
      SSB <- rbind(SSB,xx$Sbio[-n]/xx$Sbio[1])      
      alpha <-rbind(alpha,xx$alpha[2])
      beta <- rbind(beta,xx$beta[2]/xx$Sbio[1])
      sigmaR <- rbind(sigmaR,xx$sigma.r)
   }
 
    # rownames(steep) <- names(data.objects)

   years<- xx$years[1]:xx$years[2]
   par(mfrow=c(2,1),mar=c(3,3,1,1))
   matplot(years,t(R),type="l",xlab="Year",ylab="Recruitment",mgp=c(2,0.5,0))
   abline(v=1950)
   abline(v=1970)
   abline(v=1990)
 
   matplot(years,t(SSB),type="l",xlab="Year",ylab="TRO",mgp=c(2,0.5,0),ylim=c(0,max(SSB)))
   abline(v=1950)
   abline(v=1970)
   abline(v=1990)
   par(new=T, usr=c(0, 1, 0, 1))
   text(0.6,0.9,"", cex=1.2)

   windows()
   par(mfrow=c(4,3),mar=c(2,2,0,1),oma=c(2,2,0,0),mgp=c(2,0.5,0))

   ss<- seq(0,1,0.01)
   for (m1 in sort(unique(M1)))
   {
     for (m10 in sort(unique(M10)))
     {
       id <- M1==m1 & M10 == m10
       matplot(t(SSB[id,]),t(R[id,]),xlim=c(0,1),ylim=c(0,max(R)),type="l",ylab="",xlab="")
       aa <- alpha[id]*exp(0.5*sigmaR[id])
       bb <- beta[id]

      for (i in 1:length(bb))
      {
        rr <- aa[i]*ss/(bb[i]+ss)
        lines(ss,rr)
      }
      par(new=T, usr=c(0, 1, 0, 1))
      text(0.05,0.94,paste("M1=",m1),adj=0)
      text(0.05,0.85,paste("M10=",m10),adj=0)
     }
  }
   mtext("SSB",1,0.8,cex=1.3,outer=T)
   mtext("Recruitment",2,0.7,cex=1.3,outer=T)
}
