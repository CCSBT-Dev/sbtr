#' @title Plot to look at the trade-off between biomass and catch performance of MPs
#' @description
#' Plot to look at the trade-off between biomass and catch performance using the results obtained from the 3 levels of tuning.  Results for all 3 tuning levels were only obtained using a TAC-interval of 3-years starting in 2008 (option "b")  so these are the results plotted.  The specific  catch and biomass performance statistics to be compared are specified by the user.
#' @export
#' 
tradeoff.3tuning.f <- function(drs,list.vers,model,C.pstat="C.20yr.avg",B.pstat="B2022.2004",C.quantile=50,
  B.quantile=50,path="c:\\temp",par.on=T,legend.on=T,title.on=T,show.lines="b",xlim,ylim)
{
#Arguments to function are:
#1) drs: a vector giving the decision rules to be compared;
#    eg. c("CMP_1","CMP_2","CMP_3")
#2) vers: a list of vectors. Each vector is the same catch schedule but for different tuning levels and 
#        will be plotted on a separate plot, e.g. list(c("2b","3b"),c("2e","3e"))
#3) model: model to be plotted, e.g. "refset"
#2) C.pstat: catch performance statistic to use (default="C.20yr.avg")
#3) B.pstat: biomass performance statistic to use (default="B2022.2004")
#4) C.quantile: 10, 50 or 90th percentile for the C indicator (default=50)
#5) B.quantile: 10, 50 or 90th percentile for the B indicator (default=50)
#6) path: path specifying where the decision rule folders sit (ie. where the tree starts),
#7) show.lines: "b" plot points and lines, "l" lines only, "p" points only 
#8) xlim: limit for x axis fpr subgraphs, e.g. c(0,1)
#9) ylim: limit for y axis for subgraphs, e.g. c(5000, 20000)

	.Options$warn <- -1  # suppress annoying warning messages
	
	path.MP <- path
	
	nlistvers <- length(list.vers)
	windows(width=11,height=8)
	plot.arrange <- c(nlistvers/2,nlistvers/2)
	if (nlistvers==1) {plot.arrange <-c(1,1)}
	if (nlistvers==2) {plot.arrange <-c(2,1)}
	if (nlistvers==4) {plot.arrange <-c(2,2)}
	if (nlistvers==6) {plot.arrange <-c(2,3)}

	par(mfrow=plot.arrange,mai=c(0,0,0,0),omi=c(1,1,.1,.1),cex.axis=.9,cex.main=1.1)
	
	for (v in 1:nlistvers)
	{
		vers <- list.vers[[v]]
		
		ndr <- length(drs)
		nver <- length(vers)
		C.pstat.data <- matrix(0,ndr,nver)
		B.pstat.data <- matrix(0,ndr,nver)
		
		for(i in 1:ndr){ 
		 for(j in 1:nver){
		  ver <- vers[j]
		  path <- paste(path.MP,"/",drs[i],"/v",substr(vers[j],1,1),"/",sep="")
		  sumfile <- paste(path,paste(drs[i],ver,model,sep="_"),".all",sep="")
		  tmp <- read.table(sumfile,skip=3,header=F,col.names=c("Scenario","C.5yr.avg","C.10yr.avg","C.20yr.avg","C.28yr.avg","propAS","B2009.2004","B2014.2004","B2022.2004","B2032.2004","B2020.1980","B2032.1980","AAV","MinB.2004","B2020.Bmsy","B2032.Bmsy","C.TB.ratio","Astat","TAC.changes","dB.dTAC","Min.dTAC","Bslope","MinCPUE.2004","B2022","B2032","Bmsy","B0","MSY","R0","alpha","beta"))
		  wormfile <- paste(path,paste(drs[i],ver,model,sep="_"),".s3",sep="")
		  worms <- read.table(wormfile,skip=3)
		  worms <- as.matrix(worms[,-c(1,2)])
		  nyr <- (ncol(worms)-1)/2
		  bworms <- worms[,1:(1+nyr)] 
		  cworms <- worms[,(2+nyr):(2*nyr+1)]
		  tmp$B2022.2008 <- bworms[,(2022-2004+1)]/bworms[,(2008-2004+1)]
		  tmp$year.minB <- apply(bworms,1,function(x) c(2004:(2004+nyr))[x==min(x)][1] )
		  tmp$C.last10yr.avg <- apply(cworms[,(ncol(cworms)-9):ncol(cworms)],1,mean)
		  tmp$C.last20yr.avg <- apply(cworms[,(ncol(cworms)-19):ncol(cworms)],1,mean)
		  #TB: this no longer works
		  #if(B.pstat=="B2022.Bstar2022" | C.pstat=="B2022.Bstar2022"){
		  #  path <- paste(path.MP,"\\CON_01\\v0\\",sep="")
		  #  sumfile <- paste(path,paste("CON_01","0b","Cfull2",sep="_"),".all",sep="")
		  #  no.catch <- read.table(sumfile,skip=3,header=F,col.names=c("Scenario","C.5yr.avg","C.10yr.avg","C.20yr.avg","C.28yr.avg","propAS","B2009.2004","B2014.2004","B2022.2004","B2032.2004","B2020.1980","B2032.1980","AAV","MinB.2004","B2020.Bmsy","B2032.Bmsy","C.TB.ratio","Astat","TAC.changes","dB.dTAC","Min.dTAC","Bslope","MinCPUE.2004","B2022","B2032","Bmsy","B0","MSY","R0","alpha","beta"))
		  #  Bstar2022 <- no.catch$B2022  # Bstar2022 = B2022 under no catch
		  #  tmp$B2022.Bstar2022 <- tmp$B2022/Bstar2022[1:nrow(tmp)]
		  #}
		  # calculate risk statistic if required
		  if(B.pstat=="risk" | C.pstat=="risk"){
		    if(i==1 & j==1){
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
		    print(tmp$risk)
		  }
		  pstats.dat <- tmp
		  n<- nrow(pstats.dat)
		  if(B.quantile==10) B.pstat.data[i,j] <- sort(pstats.dat[,B.pstat])[max(1,trunc(.1*n))]
		  if(B.quantile==50) B.pstat.data[i,j] <- sort(pstats.dat[,B.pstat])[ceiling(.5*n)]
		  if(B.quantile==90) B.pstat.data[i,j] <- sort(pstats.dat[,B.pstat])[min(n,ceiling(.9*n)+1)]
		  if(C.quantile==10) C.pstat.data[i,j] <- sort(pstats.dat[,C.pstat])[max(1,trunc(.1*n))]
		  if(C.quantile==50) C.pstat.data[i,j] <- sort(pstats.dat[,C.pstat])[ceiling(.5*n)]
		  if(C.quantile==90) C.pstat.data[i,j] <- sort(pstats.dat[,C.pstat])[min(n,ceiling(.9*n)+1)]
		 }
		}
		
		dimnames(C.pstat.data) <- list(drs,vers)
		dimnames(B.pstat.data) <- list(drs,vers)
		if (par.on==T) {
			windows(width=10,height=8.5)
			par(mai=c(.8,1.1,.8,.4))
			par(cex.axis=.7,cex.main=1)
		}
		
		if (missing(xlim)) {
			xlim <- vector(length=2)
			xlim[1] <- min(B.pstat.data)
			xlim[2] <- max(B.pstat.data)
			xlim[1] <- xlim[1]-.1*(xlim[2]-xlim[1])
			xlim[2] <- xlim[2]+.1*(xlim[2]-xlim[1])
		}
		if (missing(ylim)) {
			ylim <- vector(length=2)
			ylim[1] <- min(C.pstat.data)
			ylim[2] <- max(C.pstat.data)
			ylim[1] <- ylim[1]-.1*(ylim[2]-ylim[1])
			ylim[2] <- ylim[2]+.1*(ylim[2]-ylim[1])
		}
		
		plot(B.pstat.data,C.pstat.data,xlim=xlim,ylim=ylim,
		  xlab="",ylab="",las=1,type="n",xaxt="n",yaxt="n")
		if (v %in% c(1,round(1+nlistvers/2))) {
			mtext(outer=T,paste(    C.quantile,"th percentile of ",C.pstat,sep=""),line=4,side=2)
			axis(2,las=1)
		}
		if (v > nlistvers/2 ) {
			mtext(paste(B.quantile,"th percentile of ",B.pstat,sep=""),outer=T,line=3.5,side=1)
			axis(1,las=1)
		}	
		if ((title.on==T)&& (v==1)) {
			mtext(side=3, "Tradeoff in biomass and catch performance over different catch schedules", outer=T, line=1)
		}
		for(i in 1:ndr) lines(B.pstat.data[i,],C.pstat.data[i,],type=show.lines,pch=i,col=i,lwd=2,cex=1.4)
		
		#abline(h=seq(0,ylim[2],0.25),col="black",lty="dotted")
		if((legend.on==T) && (v==1)) {
			legend(xlim[1],ylim[1]+(ylim[2]-ylim[1])*0.45,drs,pch=1:ndr,col=1:ndr,cex=1.1)
		}
		text(mean(xlim), ylim[1]+0.9*diff(ylim),substr(vers[1],1,10),cex=2)
	}
}
