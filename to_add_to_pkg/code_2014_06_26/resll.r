############################################################################
#Fit of the model sbtmodxx.tpl to the data for southern bluefin tuna.
#Predicted length frequency for longline fishery 1 to the observed data.
#Required the output file _lab.rep.
#Requires library PBSmodelling, available from 
#Programmed by Trevor A. Branch 29 June 2009
#outputs from a _lab.rep file, assuming that the naming convention in the file has sections of 
#outputs separated by $name comments so that it can be easily parsed by readList() function of Jon Schnute 
#IMPORTANT: library PBSmodelling required, available at http://code.google.com/p/pbs-software/downloads/list
#then in R go to menu Packages, Install Package from Local zip file
#Run by:
###source("ResLL.r")
##########################################################################
ResLL <- function(labrep.file="example_lab.rep", case_label="c1s1l1sqrt_h1m1M1O1C2a1") 
{
  library(PBSmodelling)  #for readList
  library(gplots)        #for rich.colors()
  bp<-function(x,y,v, scale=3, ...)
  {
     plot(x,y,cex=sqrt(abs(v))*scale, col=ifelse(v<0,'seagreen3','blue'), pch=ifelse(v<0,16,1), ...)
     points(x[v>0],y[v>0],cex=sqrt(v[v>0])*scale, col='blue', pch=1, ...)
  }
  x <- readList(labrep.file)
  attach(x)
  for (f in 1:4) 
  {
    yrs=len.res[len.res[,1]==f,2]
    yrtmp=rep(yrs,rep(25,length(yrs)))
    restmp=as.vector(t(len.res[len.res[,1]==f,3:27]))
    lentmp =rep( seq(from=lengths[1],by=lengths[2],length.out=lengths[3]),length(yrs ) )
    bp(yrtmp,lentmp,restmp, ylim=c(86,185), xlim=range(yrtmp), las=1, xlab='Year', ylab='Length (cm)', main=paste("Longline ",f),scale=3)
    mtext(side=3,outer=T,line=0.2,paste("(",case_label,")",sep=""),cex=0.6)
#    points(cur[,1],cur[,3],cex=sqrt(abs(cur[,6]))*scale, col='red', pch=1)
  } 

  yrs=age.res_1[,2]
  yrtmp=rep(yrs,rep(23,length(yrs)))
  restmp=as.vector(t(age.res_1[,3:25]))
  lentmp = rep( seq(from=8,by=1, length.out=23), length(yrs) )
  bp(yrtmp, lentmp, restmp, ylim=c(8,30), xlim=range(yrtmp), las=1, xlab="Year", ylab="Age", main="Indonesian", scale=3)
  mtext(side=3,outer=T,line=0.2,paste("(",case_label,")",sep=""),cex=0.6)

  yrs   = age.res_2[,2]
  yrtmp = rep(yrs,rep(8,length(yrs)))
  restmp= as.vector(t(age.res_2[,3:10]))
  lentmp= rep( seq(from=1,by=1, length.out=8), length(yrs) )
  bp(yrtmp,lentmp-1,restmp, ylim=c(0,8), xlim=range(yrtmp), las=1, xlab='Year', ylab='Age', main="Australian ",scale=3)
  mtext(side=3,outer=T,line=0.2,paste("(",case_label,")",sep=""),cex=0.6)

  detach(x)

}
#    base<-fit.base$res[fit.base$res[,2]==f,]
#    cur<-fit.current$res[fit.current$res[,2]==f,]
#    scale<-3
#    bp(base[,1],base[,3],base[,6], ylim=c(0.5,7.5), xlim=range(fit.base$years), 
#       las=1, xlab='year', ylab='Age', main=fleet.names[f], scale=scale)
#    points(cur[,1],cur[,3],cex=sqrt(abs(cur[,6]))*scale, col='red', pch=1)
#  par(op)
pdf("figs\\ResLL.pdf",width=11.5, height=8.)
ResLL(labrep.file="example_lab.rep",case_label="test")
dev.off()
