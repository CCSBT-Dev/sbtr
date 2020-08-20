######################################################
# data fits for a single lab rep run #################
######################################################
# R Hillary, CSIRO 2017 ##############################
######################################################

library(PBSmodelling)

# get the best fitting grid cell

grdnm <- 'base2016sqrt_2016'
load(paste("../filestore", paste(grdnm, "RData", sep="."),sep="/"))

lg <- length(data)
loglmat <- matrix(nrow=lg,ncol=2)
for(g in 1:lg) loglmat[g,] <- c(data[[g]][['ObjF']],data[[g]][['scenario_number']])
lmin <- min(loglmat[,1])
for(g in 1:lg) {
  if(loglmat[g,1] == lmin) {
    gmin <- g
  }
}

rep <- data[[gmin]]

########
# CPUE #
########

cv.cpue <- 0.2
plot(rep$yrs.cpue[1]:rep$yrs.cpue[2],rep$cpue,xlab='year',ylab='CPUE',type='p',pch=19,col='magenta')
lines(rep$yrs.cpue[1]:rep$yrs.cpue[2],rep$cpue.pred,lty=1,lwd=2,col='blue')
lines(rep$yrs.cpue[1]:rep$yrs.cpue[2],rep$cpue.pred*exp(-1.96*sqrt(log(1+cv.cpue^2))),lty=2,lwd=2,col='blue')
lines(rep$yrs.cpue[1]:rep$yrs.cpue[2],rep$cpue.pred*exp(1.96*sqrt(log(1+cv.cpue^2))),lty=2,lwd=2,col='blue')

#################
# aerial survey #
#################

cv.as <- 0.22
as.tab <- read.table("~/Projects/CCSBT/2017/OMMP/data/mean_scaled_AS.dat")
names(as.tab) <- c("Year","Index","CV")
as.obs <- rep$Aerial.Surv[,'obs']
as.obs[as.obs == -999] <- NA
as.pred <- rep$Aerial.Surv[,'pred']
as.lq <- rep$Aerial.Surv[,'pred']*exp(-1.96*sqrt(log(1+as.tab$CV^2)+log(1+cv.as^2)))
as.uq <- rep$Aerial.Surv[,'pred']*exp(1.96*sqrt(log(1+as.tab$CV^2)+log(1+cv.as^2)))
as.yrs <- rep$Aerial.Surv[,'year']

amax <- max(c(max(as.obs,na.rm=T),max(as.uq,na.rm=T)))
plot(as.yrs,as.obs,xlab='year',ylab='Aerial Survey',ylim=c(0,amax),type='p',pch=19,col='magenta')
lines(as.yrs,as.pred,lty=1,lwd=2,col='blue')
lines(as.yrs,as.lq,lty=2,lwd=2,col='blue')
lines(as.yrs,as.uq,lty=2,lwd=2,col='blue')

########
# tags #
########

# pooled

source("Tagging fits pooled.r")
tagging.fits.pooled(rep,ages=2:8,years=1992:1997,case_label=grdnm)

# disaggregated

source("Tagging fits combine tagger groups.r")
tagging.fits(rep,case_label=grdnm)

########
# POPs #
########

phi <- rep$phi
S <- rep$Sbio

source("sumofbinomPOP.R")
ck.om.df <- read.table("../../dev/newPOPs.dat")
names(ck.om.df) <- c("c","y","a","nP","nC")
cohorts <- sort(unique(ck.om.df$c))
aduyr <- sort(unique(ck.om.df$y))
adua <- sort(unique(ck.om.df$a))

yrs <- rep$years[1]:(rep$years[2]+1)
ages <- rep$ages[1]:rep$ages[2]

names(S) <- yrs
dimnames(phi) <- list(ages,yrs)
muPOP <- array(dim=c(length(cohorts),length(aduyr)))
sdPOP <- array(dim=c(length(cohorts),length(aduyr)))
dimnames(muPOP) <- list(cohorts,aduyr)
dimnames(sdPOP) <- dimnames(muPOP)
POPs.obs <- tapply(ck.om.df$nP,list(ck.om.df$c,ck.om.df$y),sum)
COMs.obs <- tapply(ck.om.df$nC,list(ck.om.df$c,ck.om.df$y),sum)


for(cc in cohorts[1]:cohorts[length(cohorts)]) {
    for(yy in aduyr[1]:aduyr[length(aduyr)]) {

      cat("Cohort: ",cc," Year: ",yy,"\n")

      xx.df <- subset(ck.om.df,c == cc & y == yy)

      if(dim(xx.df)[1] > 0) {

        # no. of non-zero records
        nrec <- dim(xx.df)[1]
        #######################################################
        # Begin Butler-Stephens algorithm to get summed probs #
        #######################################################
        psum <- sumofbinomPOP(xx.df,S,phi)
        mutmp <- sum(psum*(0:(length(psum)-1)))
        muPOP[as.character(cc),as.character(yy)] <- mutmp
        sdPOP[as.character(cc),as.character(yy)] <- sqrt(sum(psum*((0:(length(psum)-1))-mutmp)^2))
      }
    }
}

# create summary data frame

ck.summ.df <- expand.grid(cohort=cohorts,year=aduyr,obs=NA,med=NA,uq=NA,lq=NA)
for(i in 1:dim(ck.summ.df)[1]) {
  cc <- ck.summ.df$cohort[i]
  yy <- ck.summ.df$year[i]
  if(!is.na(COMs.obs[as.character(cc),as.character(yy)])) {
    ck.summ.df$obs[i] <- POPs.obs[as.character(cc),as.character(yy)]
    ck.summ.df$med[i] <- muPOP[as.character(cc),as.character(yy)]
    ck.summ.df$lq[i] <- ck.summ.df$med[i]-1.96*sdPOP[as.character(cc),as.character(yy)]
    ck.summ.df$lq[i] <- max(0,ck.summ.df$lq[i])
    ck.summ.df$uq[i] <- ck.summ.df$med[i]+1.96*sdPOP[as.character(cc),as.character(yy)]
  }
}

# plot em

library(lattice)
library(ggplot2)

xyplot(obs+med+lq+uq~cohort|as.factor(year),ck.summ.df,type=c('p','l','l','l'),lty=c(0,1,2,2),col=c('magenta','blue','blue','blue'),pch=c(19,NA,NA,NA),cex=0.75,lwd=1.5,xlab='cohort',ylab='POPs')

# ggplot() version

p1 <- ggplot(ck.summ.df,aes(cohort,obs) + geom_point() + facet_grid(~year))

# cohort-specific summary (across adult ages and years)

muPOPc <- sdPOPc <- vector(length = length(cohorts))
names(muPOPc) <- names(sdPOPc) <- cohorts
POPs.obsc <- tapply(ck.om.df$nP,list(ck.om.df$c),sum)
COMs.obsc <- tapply(ck.om.df$nC,list(ck.om.df$c),sum)

for(cc in cohorts[1]:cohorts[length(cohorts)]) {

   xx.df <- subset(ck.om.df,c == cc)

   if(dim(xx.df)[1] > 0) {

        # no. of non-zero records
        nrec <- dim(xx.df)[1]
        #######################################################
        # Begin Butler-Stephens algorithm to get summed probs #
        #######################################################
        psum <- sumofbinomPOP(xx.df,S,phi)
        mutmp <- sum(psum*(0:(length(psum)-1)))
        muPOPc[as.character(cc)] <- mutmp
        sdPOPc[as.character(cc)] <- sqrt(sum(psum*((0:(length(psum)-1))-mutmp)^2))
   }

   cat("Cohort:",cc,"\n")
}

ck.summc.df <- expand.grid(cohort=cohorts,obs=NA,med=NA,uq=NA,lq=NA)
for(i in 1:dim(ck.summc.df)[1]) {
  cc <- ck.summc.df$cohort[i]
  if(!is.na(COMs.obsc[as.character(cc)])) {
    ck.summc.df$obs[i] <- POPs.obsc[as.character(cc)]
    ck.summc.df$med[i] <- muPOPc[as.character(cc)]
    ck.summc.df$lq[i] <- ck.summc.df$med[i]-1.96*sdPOPc[as.character(cc)]
    ck.summc.df$lq[i] <- max(0,ck.summc.df$lq[i])
    ck.summc.df$uq[i] <- ck.summc.df$med[i]+1.96*sdPOPc[as.character(cc)]
  }
}

xyplot(obs+med+lq+uq~cohort,ck.summc.df,type=c('p','l','l','l'),lty=c(0,1,2,2),col=c('magenta','blue','blue','blue'),pch=c(19,NA,NA,NA),cex=0.75,lwd=1.5,xlab='cohort',ylab='POPs')

# now predicted and observed POPs for adult ages

muPOPa <- sdPOPa <- vector(length = length(adua))
names(muPOPa) <- names(sdPOPa) <- adua
POPs.obsa <- tapply(ck.om.df$nP,list(ck.om.df$a),sum)
COMs.obsa <- tapply(ck.om.df$nC,list(ck.om.df$a),sum)

for(aa in adua[1]:adua[length(adua)]) {

   xx.df <- subset(ck.om.df,a == aa)

   if(dim(xx.df)[1] > 0) {

        # no. of non-zero records
        nrec <- dim(xx.df)[1]
        #######################################################
        # Begin Butler-Stephens algorithm to get summed probs #
        #######################################################
        psum <- sumofbinomPOP(xx.df,S,phi)
        mutmp <- sum(psum*(0:(length(psum)-1)))
        muPOPa[as.character(aa)] <- mutmp
        sdPOPa[as.character(aa)] <- sqrt(sum(psum*((0:(length(psum)-1))-mutmp)^2))
   }

   cat("Adult age:",aa,"\n")
}

ck.summa.df <- expand.grid(age=adua,obs=NA,med=NA,uq=NA,lq=NA)
for(i in 1:dim(ck.summa.df)[1]) {
  aa <- ck.summa.df$age[i]
  if(!is.na(COMs.obsa[as.character(aa)])) {
    ck.summa.df$obs[i] <- POPs.obsa[as.character(aa)]
    ck.summa.df$med[i] <- muPOPa[as.character(aa)]
    ck.summa.df$lq[i] <- ck.summa.df$med[i]-1.96*sdPOPa[as.character(aa)]
    ck.summa.df$lq[i] <- max(0,ck.summa.df$lq[i])
    ck.summa.df$uq[i] <- ck.summa.df$med[i]+1.96*sdPOPa[as.character(aa)]
  }
}

xyplot(obs+med+lq+uq~age,subset(ck.summa.df,age<=30),type=c('p','l','l','l'),lty=c(0,1,2,2),col=c('magenta','blue','blue','blue'),pch=c(19,NA,NA,NA),cex=0.75,lwd=1.5,xlab='adult capture age',ylab='POPs')


########
# HSPs #
########

# save it

save.image(paste("../filestore",paste(rep$scenario_number,"rda",sep="."),sep="/"))

