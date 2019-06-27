#' @title Plot to look at the trade-off between biomass and catch performance of MPs
#' @description
#' Plot to look at the trade-off between biomass and catch performance of numerous MPs, intended for one level of tuning but doesn't have to be. The specific catch and biomass performance statistics and quantiles to be compared are specified by the user.
#' @export
#' 
tradeoff.f <- function(dr.ver,B.pstat="B2022.2004",C.pstat="C.20yr.avg",B.quantile=50, model="Cfull2",
  C.quantile=50,xlimits,ylimits,path="local")
{
#Arguments to function are:
#1) dr.ver: a vector giving the decision rules and versions which are to 
#     be compared; eg. c("CMP_1","2b","CMP_2","2b","CONST","2b")
#2) B.pstat: performance statistic to plot on x-axis (default="B2022.2004")
#3) C.pstat: performance statistic to plot on y-axis (default="C.20yr.avg")
#4) B.quantile: 10, 50 or 90th percentile for the B.pstat (default=50)
#5) C.quantile: 10, 50 or 90th percentile for the C.pstat (default=50)
#6) path: path specifying where the decision rule folders sit (ie. where the tree starts),
#     either "local" (c:\\data\\MP\\tree") or "shared" ("s:\\tropical and pelagic\\pelagic ecosystems\\tuna\\ccsbt-mp\\tree")

.Options$warn <- -1  # suppress annoying warning messages

path.MP <- path

ndr <- length(dr.ver)/2
dr <- vector(mode="character",length=ndr)
ver <- vector(mode="character",length=ndr)
for(i in 1:ndr){
    dr[i] <- dr.ver[1+2*(i-1)]
    ver[i] <- dr.ver[2+2*(i-1)]
}

legend.dr <- dr
legend.ver <- ver

#legend.dr <- c(dr,"Zero catch")
#legend.ver <- c(ver,"")

#dr <- c(dr,"CON_01")
#ver <- c(ver,"0b")
#ndr <- ndr+1 

B.data <- rep(0,ndr)
C.data <- rep(0,ndr)

for(i in 1:ndr){ 
  tuning.level <- as.numeric(substring(ver[i],1,1))
  path <- paste(path.MP,"\\",dr[i],"\\v",tuning.level,"\\",sep="")
  sumfile <- paste(path,paste(dr[i],ver[i],model,sep="_"),".all",sep="")
  tmp <- read.table(sumfile,skip=3,header=F,col.names=c("Scenario","C.5yr.avg","C.10yr.avg","C.20yr.avg","C.28yr.avg","propAS","B2009.2004","B2014.2004","B2022.2004","B2032.2004","B2020.1980","B2032.1980","AAV","MinB.2004","B2020.Bmsy","B2032.Bmsy","C.TB.ratio","Astat","TAC.changes","dB.dTAC","Min.dTAC","Bslope","MinCPUE.2004","B2022","B2032","Bmsy","B0","MSY","R0","alpha","beta"))
  wormfile <- paste(path,paste(dr[i],ver[i],model,sep="_"),".s3",sep="")
  worms <- read.table(wormfile,skip=3)
  worms <- as.matrix(worms[,-c(1,2)])
  nyr <- (ncol(worms)-1)/2
  bworms <- worms[,1:(1+nyr)] 
  cworms <- worms[,(2+nyr):(2*nyr+1)]
  tmp$B2022.2008 <- bworms[,(2022-2004+1)]/bworms[,(2008-2004+1)]
  tmp$year.minB <- apply(bworms,1,function(x) c(2004:(2004+nyr))[x==min(x)][1] )
  tmp$C.last10yr.avg <- apply(cworms[,(ncol(cworms)-9):ncol(cworms)],1,mean)
  tmp$C.last20yr.avg <- apply(cworms[,(ncol(cworms)-19):ncol(cworms)],1,mean)
  tmp$"mean C2008-13" <- apply(cworms[,5:10],1,mean)
  
  if(B.pstat=="B2022.Bstar2022" | C.pstat=="B2022.Bstar2022"){
      path <- paste(path.MP,"\\CON_01\\v0\\",sep="")
      sumfile <- paste(path,paste("CON_01","0b",model,sep="_"),".all",sep="")
      no.catch <- read.table(sumfile,skip=3,header=F,col.names=c("Scenario","C.5yr.avg","C.10yr.avg","C.20yr.avg","C.28yr.avg","propAS","B2009.2004","B2014.2004","B2022.2004","B2032.2004","B2020.1980","B2032.1980","AAV","MinB.2004","B2020.Bmsy","B2032.Bmsy","C.TB.ratio","Astat","TAC.changes","dB.dTAC","Min.dTAC","Bslope","MinCPUE.2004","B2022","B2032","Bmsy","B0","MSY","R0","alpha","beta"))
      Bstar2022 <- no.catch$B2022  # Bstar2022 = B2022 under no catch
      tmp$B2022.Bstar2022 <- tmp$B2022/Bstar2022[1:nrow(tmp)]
  }
  # calculate risk statistic if required
  if(B.pstat=="risk" | C.pstat=="risk"){
      if(i==1){
        print("Enter spawning stock biomass reference (B0, Bmsy, B1980 or B2004):")
        B.ref=scan(what="character",n=1)
        print("Enter risk threshold value:")
        thresh=scan(n=1)
        print("Enter gamma value:")
        gamma=scan(n=1)
      }
      if(B.ref=="B0" | B.ref=="Bmsy") B.ref.vec <- tmp[,B.ref]
      if(B.ref=="B1980") B.ref.vec <- tmp$B2032/tmp$B2032.1980
      if(B.ref=="B2004") B.ref.vec <- tmp$B2032/tmp$B2032.2004
      frac<- bworms/B.ref.vec  #frac is a matrix with dimensions (#reps)x(#years)
      tmp$risk <- apply( ((thresh>=frac)*(thresh-frac)/thresh)^gamma,1,mean)
  }

  pstats.dat <- tmp
  n<- nrow(pstats.dat)
  if(B.quantile==10) B.data[i] <- sort(pstats.dat[,B.pstat])[max(1,trunc(.1*n))]
  if(B.quantile==50) B.data[i] <- sort(pstats.dat[,B.pstat])[ceiling(.5*n)]
  if(B.quantile==90) B.data[i] <- sort(pstats.dat[,B.pstat])[min(n,ceiling(.9*n)+1)]
  if(C.quantile==10) C.data[i] <- sort(pstats.dat[,C.pstat])[max(1,trunc(.1*n))]
  if(C.quantile==50) C.data[i] <- sort(pstats.dat[,C.pstat])[ceiling(.5*n)]
  if(C.quantile==90) C.data[i] <- sort(pstats.dat[,C.pstat])[min(n,ceiling(.9*n)+1)]
}

windows(width=10,height=8.5)
par(mai=c(.8,.8,.8,.4))
par(cex.axis=.7,cex.main=1)

if(missing(xlimits)) xlimits <- c(.975*min(B.data),1.025*max(B.data))
if(missing(ylimits)) ylimits <- c(.975*min(C.data),1.025*max(C.data))

plot(B.data,C.data,xlim=xlimits,ylim=ylimits,
  xlab=paste(B.quantile,"th percentile of ",B.pstat,sep=""),
  ylab=paste(C.quantile,"th percentile of ",C.pstat,sep=""),las=1,type="n")
for(i in 1:ndr) points(B.data[i],C.data[i],pch=i,col=i,lwd=2,cex=2)
title("Tradeoff in biomass and catch performance for selected MP's")

#abline(h=seq(0,ymax,0.25),col="black",lty="dotted")

legend(xlimits[1],ylimits[1]+diff(ylimits)*0.6,paste(legend.dr,legend.ver,sep=" "),pch=1:ndr,col=1:ndr)

}


