#' @title Plot for comparison between the 2014 assessment results.
#' @description
#' Originally for Appendix Fig 2 of CCSBT-ESC/1409/38.
#'
#' @param runname1 name of the base case result
#' @param runname2 name of the sensitivity result
#' @return a plot
#' @examples
#' Comp.Rec.B10.Cat.f(prjdir="C:\\sbtmod\\projections\\MP3_v21_", runname1="base", runname2="CKoff")
#' @export
#'
Comp.Rec.B10.Cat.f <- function(prjdir="projections\\MP3_v21_",
 runname1, runname2, prefix="2014_assess")
{

# Data #
outputf <- paste("Figs\\", prefix, "_Traj_", runname1, "_and_", runname2, ".pdf", sep="")
inputf1 <- paste(prjdir, runname1, sep="")
inputf2 <- paste(prjdir, runname2, sep="")

hist_start_yr <- 1931
hist_end_yr <- 2013
prj_start_yr <- 2014
prj_end_yr <- 2014+27-1
tot_yr <- prj_end_yr-hist_start_yr+1 
nhist_yr <- hist_end_yr-hist_start_yr+1
nprj_yr <- prj_end_yr- hist_end_yr


# Sensitivity Biomass #
#file1
tmp4 <- read.table(file=paste(inputf1, ".s4", sep=""), skip=2, head=T)
head(tmp4,5)
colnames(tmp4)
tmp5 <- tmp4[,(3+(tot_yr+1)*2):(2+(tot_yr+1)*2+tot_yr)]
med <- NULL
q05 <- NULL
q95 <- NULL
for(i in 1:(length(tmp5[1,]))) 
	{
	tmp5med <- median(tmp5[,i])
	med <- append(med, tmp5med)
	tmp5q05 <- quantile(tmp5[,i],0.05)
	q05 <- append(q05, tmp5q05)
	tmp5q95 <- quantile(tmp5[,i],0.95)
	q95 <- append(q95, tmp5q95)
	}
tmp6 <- NULL
tmp6 <- cbind(med,q05)
tmp6 <- cbind(tmp6,q95)
rownames(tmp6) <- colnames(tmp5)
New_Bio1 <- tmp6

#file2
tmp4 <- read.table(file=paste(inputf2, ".s4", sep=""), skip=2, head=T)
head(tmp4,5)
colnames(tmp4)
tmp5 <- tmp4[,(3+(tot_yr+1)*2):(2+(tot_yr+1)*2+tot_yr)]
med <- NULL
q05 <- NULL
q95 <- NULL
for(i in 1:(length(tmp5[1,]))) 
	{
	tmp5med <- median(tmp5[,i])
	med <- append(med, tmp5med)
	tmp5q05 <- quantile(tmp5[,i],0.05)
	q05 <- append(q05, tmp5q05)
	tmp5q95 <- quantile(tmp5[,i],0.95)
	q95 <- append(q95, tmp5q95)
	}
tmp6 <- NULL
tmp6 <- cbind(med,q05)
tmp6 <- cbind(tmp6,q95)
rownames(tmp6) <- colnames(tmp5)
New_Bio2 <- tmp6


# New assess Sp_Bio_index #
#file1
tmp1 <- read.table(file=paste(inputf1, ".s4", sep=""), skip=2, head=T)
head(tmp1,5)
colnames(tmp1)
tmp2 <- tmp1[,3:(2+tot_yr)]
med <- NULL
q05 <- NULL
q95 <- NULL
for(i in 1:(length(tmp2[1,]))) 
	{
	tmp2med <- median(tmp2[,i])
	med <- append(med, tmp2med)
	tmp2q05 <- quantile(tmp2[,i],0.05)
	q05 <- append(q05, tmp2q05)
	tmp2q95 <- quantile(tmp2[,i],0.95)
	q95 <- append(q95, tmp2q95)
	}
tmp3 <- NULL
tmp3 <- cbind(med,q05)
tmp3 <- cbind(tmp3,q95)
rownames(tmp3) <- colnames(tmp2)
New_SSBindex1 <- tmp3

#file2
tmp1 <- read.table(file=paste(inputf2, ".s4", sep=""), skip=2, head=T)
head(tmp1,5)
colnames(tmp1)
tmp2 <- tmp1[,3:(2+tot_yr)]
med <- NULL
q05 <- NULL
q95 <- NULL
for(i in 1:(length(tmp2[1,]))) 
	{
	tmp2med <- median(tmp2[,i])
	med <- append(med, tmp2med)
	tmp2q05 <- quantile(tmp2[,i],0.05)
	q05 <- append(q05, tmp2q05)
	tmp2q95 <- quantile(tmp2[,i],0.95)
	q95 <- append(q95, tmp2q95)
	}
tmp3 <- NULL
tmp3 <- cbind(med,q05)
tmp3 <- cbind(tmp3,q95)
rownames(tmp3) <- colnames(tmp2)
New_SSBindex2 <- tmp3


# New assess Recruitment #
#file1
tmp1 <- read.table(file=paste(inputf1, ".s4", sep=""), skip=2, head=T)
tmp2 <- tmp1[,(4+tot_yr):(3+tot_yr*2)]
med <- NULL
q05 <- NULL
q95 <- NULL
for(i in 1:(length(tmp2[1,]))) 
	{
	tmp2med <- median(tmp2[,i])
	med <- append(med, tmp2med)
	tmp2q05 <- quantile(tmp2[,i],0.05)
	q05 <- append(q05, tmp2q05)
	tmp2q95 <- quantile(tmp2[,i],0.95)
	q95 <- append(q95, tmp2q95)
	}
tmp3 <- NULL
tmp3 <- cbind(med,q05)
tmp3 <- cbind(tmp3,q95)
rownames(tmp3) <- colnames(tmp2)
New_Rec1 <- tmp3

#file2
tmp1 <- read.table(file=paste(inputf2, ".s4", sep=""), skip=2, head=T)
tmp2 <- tmp1[,(4+tot_yr):(3+tot_yr*2)]
med <- NULL
q05 <- NULL
q95 <- NULL
for(i in 1:(length(tmp2[1,]))) 
	{
	tmp2med <- median(tmp2[,i])
	med <- append(med, tmp2med)
	tmp2q05 <- quantile(tmp2[,i],0.05)
	q05 <- append(q05, tmp2q05)
	tmp2q95 <- quantile(tmp2[,i],0.95)
	q95 <- append(q95, tmp2q95)
	}
tmp3 <- NULL
tmp3 <- cbind(med,q05)
tmp3 <- cbind(tmp3,q95)
rownames(tmp3) <- colnames(tmp2)
New_Rec2 <- tmp3


#####################################################################################################################

# Plot #
pdf(outputf, width=7, height=10, paper="A4")
# runname1: base run
#pol.hist.col1 <- "#FF149390"
#pol.proj.col1 <- "#FFC0CB90"
#bor.col1 <- "#FF1493"
#lin.col1 <- "red"

# runname1: base run
pol.hist.col1 <- "#99999970"
pol.proj.col1 <- "#99999970"
bor.col1 <- "#91919180"
lin.col1 <- "#91919180"


# runname2: main run
pol.hist.col2 <- "#008B0070"
pol.proj.col2 <- "#ADFF2F70"
bor.col2 <- "#008B0080"
lin.col2 <- "#228B2280"
 
#nf <- layout(matrix(c(1,3,2,4),2,2,byrow=T), c(2,1), c(1.2,1.2), TRUE)
nf <- layout(matrix(c(1,3,2,4),2,2,byrow=T), c(2,1), c(0.95,0.95), TRUE)
#layout.show(nf)   # If you want show layout of figs....

#Fig1
par(mar=c(2.5,4.0,0.5,0.5))

## RECRUIT ##
dnum <- length(New_Rec2[,1])
plot(New_Rec2[,3], type="n", xlab="Year", ylab="Recruit (N)", ylim=c(0, max(New_Rec2[,3])), axes=F)

# base_hist
polygon(c(1:(nhist_yr),(nhist_yr):1), c(New_Rec1[1:(nhist_yr),2],New_Rec1[(nhist_yr):1,3]),border=F, col=pol.hist.col1)	
# base_proj
polygon(c((nhist_yr):tot_yr,tot_yr:(nhist_yr)),c(New_Rec1[(nhist_yr):tot_yr,2],New_Rec1[tot_yr:(nhist_yr),3]),border=F, col=pol.proj.col1)	
# base
points(c(1:dnum), New_Rec1[1:dnum,1], type="l", lwd=2, col=lin.col1)
abline(v=(nhist_yr), col=bor.col1, lty=3, lwd=2)

# new_hist
polygon(c(1:(nhist_yr),(nhist_yr):1), c(New_Rec2[1:(nhist_yr),2],New_Rec2[(nhist_yr):1,3]),border=F, col=pol.hist.col2)	
# new_proj
polygon(c((nhist_yr):tot_yr,tot_yr:(nhist_yr)),c(New_Rec2[(nhist_yr):tot_yr,2],New_Rec2[tot_yr:(nhist_yr),3]),border=F, col=pol.proj.col2)	
# new
points(c(1:dnum), New_Rec2[1:dnum,1], type="l", lwd=2, col=lin.col2)
abline(v=(nhist_yr), col=bor.col2, lty=3, lwd=2)

# axis
axis(1, pos=NA, at=seq(0,dnum,10), labels=1931+seq(0,dnum,10)-1, adj=0, cex.axis=0.8, cex.lab=0.9)
axis(2, pos=NA, at=pretty(par("usr")[3:4]), adj=1, cex.axis=0.8, cex.lab=0.9)

# legend
legend("topright", c(paste(runname1), paste(runname2)), lwd=2, col=c(lin.col1, lin.col2), bg="white")
#runname1b <- gsub("MP3_v21_", "", runname1)
#runname2b <- gsub("MP3_v21_", "", runname2)
#legend("topright", c(paste(runname1b), paste(runname2b)), lwd=2, col=c(lin.col1, lin.col2), bg="white")

legend("topleft", c("a)"), box.col=F, cex=1.2, bg=F, x.intersp=0)

box()

#####################################################################################################################


#Fig2
par(mar=c(2.5,4.0,0.5,0.5))

## BIOMASS (age10+) ##
dnum <- length(New_Bio2[,1])
plot(New_Bio2[,3], type="n", xlab="Year", ylim=c(0, max(New_Bio2[,3])), ylab="Biomass(t) of age10+", axes=F)

# base_hist
polygon(c(1:(nhist_yr),(nhist_yr):1), c(New_Bio1[1:(nhist_yr),2],New_Bio1[(nhist_yr):1,3]),border=F, col=pol.hist.col1)	
# base_proj
polygon(c((nhist_yr):tot_yr,tot_yr:(nhist_yr)),c(New_Bio1[(nhist_yr):tot_yr,2],New_Bio1[tot_yr:(nhist_yr),3]),border=F, col=pol.proj.col1)	
# base
points(c(1:dnum), New_Bio1[1:dnum,1], type="l", lwd=2, col=lin.col1)
abline(v=(nhist_yr), col=bor.col1, lty=3, lwd=2)

# new_hist
polygon(c(1:(nhist_yr),(nhist_yr):1), c(New_Bio2[1:(nhist_yr),2],New_Bio2[(nhist_yr):1,3]),border=F, col=pol.hist.col2)	
# new_proj
polygon(c((nhist_yr):tot_yr,tot_yr:(nhist_yr)),c(New_Bio2[(nhist_yr):tot_yr,2],New_Bio2[tot_yr:(nhist_yr),3]),border=F, col=pol.proj.col2)	
# new
points(c(1:dnum), New_Bio2[1:dnum,1], type="l", lwd=2, col=lin.col2)
abline(v=(nhist_yr), col=bor.col2, lty=3, lwd=2)

# axis
axis(1, pos=NA, at=seq(0,dnum,10), labels=1931+seq(0,dnum,10)-1, adj=0, cex.axis=0.8, cex.lab=0.9)
axis(2, pos=NA, at=pretty(par("usr")[3:4]), adj=1, cex.axis=0.8, cex.lab=0.9)

# legend
#legend("topright", c(paste("2014", filename), lwd=2, col=lin.col1, bg="white")
# legend
legend("topleft", c("b)"), box.col=F, cex=1.2, bg=F, x.intersp=0)

box()

#####################################################################################################################


##Fig3
par(mar=c(2.5,4.0,0.5,0.5))

worm.compCatch.f <- function(oldfilename, newfilename, n.worms=10, ylim=c(0,20), xlim=c(2013,2040), seed=1, title="", 
pol.col1="#00BFFF50", lin.col1="#4682B480", pol.col2="#FFC0CB80", lin.col2="red", markcex=1.0)
{
   #Plots n.worms (realizations), the median and the 10-90th percentiles of age 10plus biomass

   set.seed(seed)
   .Options$warn <- -1  # suppress annoying warning messages
   tmp1 <- read.table(oldfilename,skip=2,header=T)	# for s3file
   tmp2 <- read.table(newfilename,skip=2,header=T)	# for s3file
   tmp1 <- as.matrix(tmp1[,-c(1,2)])
   tmp2 <- as.matrix(tmp2[,-c(1,2)])
   nyr <- (ncol(tmp2)-1)/2
   
   #find data years
   min.yr <- floor(xlim[2])
   max.yr <- ceiling(xlim[1])
   for (yr in ceiling(xlim[1]):floor(xlim[2])) {
      if(length(grep(paste("C.",yr,sep=""),colnames(tmp2)))>=1) {
         if (yr<min.yr) {min.yr <- yr}
         if (yr>max.yr) {max.yr <- yr}
      }
   }

   data.columns <- (grep(paste("C.",min.yr,sep=""),colnames(tmp1))):(grep(paste("C.",max.yr,sep=""),colnames(tmp1)) )
   yr.first <- min.yr
   yr.last <- max.yr
   cworms1 <- tmp1[,data.columns] /1000

   data.columns <- (grep(paste("C.",min.yr,sep=""),colnames(tmp2))):(grep(paste("C.",max.yr,sep=""),colnames(tmp2)) )
   yr.first <- min.yr
   yr.last <- max.yr
   cworms2 <- tmp2[,data.columns] /1000

   n <- nrow(cworms1)
   C.quantiles1 <- apply(cworms1, 2, function(x){sort(x)[c(max(1,trunc(.1*n)), ceiling(.5*n), min(n,ceiling(.9*n)+1))]})

   n <- nrow(cworms2)
   C.quantiles2 <- apply(cworms2, 2, function(x){sort(x)[c(max(1,trunc(.1*n)), ceiling(.5*n), min(n,ceiling(.9*n)+1))]})


#   par(mfrow=c(1,1), mai=c(0.3,1,0,.2), omi=c(0.5,0,0.6,0))
   par(cex.axis=.8)

   #find some worms1
   K1 <- min(nrow(cworms1), n.worms)
   worms.index1 <- sample(1:nrow(cworms1), K1)

   #find some worms2
   K2 <- min(nrow(cworms2), n.worms)
   worms.index2 <- sample(1:nrow(cworms2), K2)

   if(missing(ylim)) ylim <- c(0,max(cworms1[worms.index1,], cworms2[worms.index2,], C.quantiles1, C.quantiles2))
   else ylim <- ylim
   
   # plot #
   plot(yr.first:yr.last, C.quantiles1[2,], ylim=ylim, xlab="",ylab="",  xlim=c(yr.first-.5,yr.last+.5), xaxt="n", cex=markcex, yaxs="i", xaxs="i", las=1)

   polygon(c(yr.first:yr.last, yr.last:yr.first), c(C.quantiles1[1,], rev(C.quantiles1[3,])), col=pol.col1, border=NA) 
   lines(yr.first:yr.last, C.quantiles1[2,], type="o", lwd=2, cex=markcex, col=lin.col1)
   if(n.worms>0) { 
     for(i in worms.index1)
      {lines(yr.first:yr.last, cworms1[i,], lwd=0.5, col=lin.col1)}
   }
   
   polygon(c(yr.first:yr.last, yr.last:yr.first), c(C.quantiles2[1,], rev(C.quantiles2[3,])), col=pol.col2, border=NA) 
   lines(yr.first:yr.last,C.quantiles2[2,], type="o", lwd=2, cex=markcex, col=lin.col2)
   if(n.worms>0) { 
     for(i in worms.index2)
       lines(yr.first:yr.last, cworms2[i,], lwd=0.5, col=lin.col2)
   }
   axis(side=1,cex=1)
#   mtext("Year",side=1,outer=F,cex=1.3,line=2.5)
   mtext(side=2,outer=F,cex=0.8,line=2.5,expression(paste("Catch (",10^3," mt)",sep="")))
   mtext(title,side=3,outer=T,line=1.3,cex=1)
}

worm.compCatch.f(paste(inputf1, ".s3", sep=""), paste(inputf2, ".s3", sep=""), xlim=c(2014,2040), 
pol.col1=pol.proj.col1, lin.col1=lin.col1, pol.col2=pol.proj.col2, lin.col2=lin.col2, markcex=0.5)
legend("topleft", c("c)"), box.col=F, cex=1.2, bg=F, x.intersp=0)


#####################################################################################################################

#Fig4
par(mar=c(2.5,4.0,0.5,0.5))

worm.compB10plus.f <- function(oldfilename, newfilename, n.worms=10, ylim=c(0,20), xlim=c(2013,2040), seed=1, title="", 
pol.col1="#00BFFF50", lin.col1="#4682B480", pol.col2="#FFC0CB80", lin.col2="red", markcex=1.0)
{
   #Plots n.worms (realizations), the median and the 10-90th percentiles of age 10plus biomass

   set.seed(seed)
   .Options$warn <- -1  # suppress annoying warning messages
   tmp1 <- read.table(oldfilename,skip=2,header=T)
   tmp2 <- read.table(newfilename,skip=2,header=T)
   
   #find data years
   min.yr <- floor(xlim[2])
   max.yr <- ceiling(xlim[1])
   for (yr in ceiling(xlim[1]):floor(xlim[2])) {
      if(length(grep(paste("TB10plus.",yr,sep=""),colnames(tmp2)))>=1) {
         if (yr<min.yr) {min.yr <- yr}
         if (yr>max.yr) {max.yr <- yr}
      }
   }

#   data.columns <- (grep(paste("B.",min.yr,sep=""),colnames(tmp1))):(grep(paste("B.",max.yr,sep=""),colnames(tmp1)) )
#   yr.first <- min.yr
#   yr.last <- max.yr
#   bworms1 <- tmp1[,data.columns] /1000000

   data.columns <- (grep(paste("TB10plus.",min.yr,sep=""),colnames(tmp1))):(grep(paste("TB10plus.",max.yr,sep=""),colnames(tmp1)) )
   yr.first <- min.yr
   yr.last <- max.yr
   bworms1 <- tmp1[,data.columns] /1000000

   data.columns <- (grep(paste("TB10plus.",min.yr,sep=""),colnames(tmp2))):(grep(paste("TB10plus.",max.yr,sep=""),colnames(tmp2)) )
   yr.first <- min.yr
   yr.last <- max.yr
   bworms2 <- tmp2[,data.columns] /1000000


   # Using the quantile function does not give the exact same anwer as Vivian
   # gets in her summary files; instead calculate quantiles the following way:

   n <- nrow(bworms1)
   B.quantiles1 <- apply(bworms1, 2, function(x){sort(x)[c(max(1,trunc(.1*n)), ceiling(.5*n), min(n,ceiling(.9*n)+1))]})

   n <- nrow(bworms2)
   B.quantiles2 <- apply(bworms2, 2, function(x){sort(x)[c(max(1,trunc(.1*n)), ceiling(.5*n), min(n,ceiling(.9*n)+1))]})


#   par(mfrow=c(1,1), mai=c(0.3,1,0,.2), omi=c(0.5,0,0.6,0))
   par(cex.axis=.8)

   #find some worms1
   K1 <- min(nrow(bworms1), n.worms)
   worms.index1 <- sample(1:nrow(bworms1), K1)

   #find some worms2
   K2 <- min(nrow(bworms2), n.worms)
   worms.index2 <- sample(1:nrow(bworms2), K2)

   if(missing(ylim)) ylim <- c(0,max(bworms1[worms.index1,], bworms2[worms.index2,], B.quantiles1, B.quantiles2))
   else ylim <- ylim
   
   # plot #
   plot(yr.first:yr.last, B.quantiles1[2,], ylim=ylim, xlab="",ylab="",  xlim=c(yr.first-.5,yr.last+.5), xaxt="n", cex=markcex, yaxs="i", xaxs="i", las=1)

   polygon(c(yr.first:yr.last, yr.last:yr.first), c(B.quantiles1[1,], rev(B.quantiles1[3,])), col=pol.col1, border=NA) 
   lines(yr.first:yr.last, B.quantiles1[2,], type="o", lwd=2, cex=markcex, col=lin.col1)
   if(n.worms>0) { 
     for(i in worms.index1)
      {lines(yr.first:yr.last, bworms1[i,], lwd=0.5, col=lin.col1)}
   }
   
   polygon(c(yr.first:yr.last, yr.last:yr.first), c(B.quantiles2[1,], rev(B.quantiles2[3,])), col=pol.col2, border=NA) 
   lines(yr.first:yr.last,B.quantiles2[2,], type="o", lwd=2, cex=markcex, col=lin.col2)
   if(n.worms>0) { 
     for(i in worms.index2)
       lines(yr.first:yr.last, bworms2[i,], lwd=0.5, col=lin.col2)
   }
   axis(side=1,cex=1)
#   mtext("Year",side=1,outer=F,cex=1.3,line=2.5)
   mtext(side=2,outer=F,cex=0.8,line=2.5,expression(paste("Biomass of age10plus ( ",10^6," mt)",sep="")))
   mtext(title,side=3,outer=T,line=1.3,cex=1)
}

worm.compB10plus.f(paste(inputf1, ".s4", sep=""), paste(inputf2, ".s4", sep=""), xlim=c(2014,2040), 
pol.col1=pol.proj.col1, lin.col1=lin.col1, pol.col2=pol.proj.col2, lin.col2=lin.col2, markcex=0.5)
legend("topleft", c("d)"), box.col=F, cex=1.2, bg=F, x.intersp=0)


dev.off()

}