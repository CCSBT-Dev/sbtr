#############################################################################
#Big table requested by the SC including important components.
#############################################################################

#Notes:
#Depending on the tuning level, the procedures are tuned to 2040, 2035, 2030, etc. Therefore a year
#   called "Year SSBt" is specified for each tuning level to compare performance of the MPs. 
#10th percentile SSBt (and Ct) is assumed to be 10th percentile of Ct in year "Year SSBt"
#

table.final <- function(prefixes,exclude=NULL) {
   nfiles <- length(prefixes)
	base.s3file.names <- paste(prefixes,".s3",sep="")
	base.s4file.names <- paste(prefixes,".s4",sep="")

	#Note that these can be trivially reordered if necessary
	results <- matrix(nrow=8, ncol=23, dimnames=list(NULL,c("Run","Tuning yr","Max increase", "Increase","MP","Year SSBt",
							"P(SSBt>0.2SSB0)","P(SSB2035>0.2SSB0)","P(SSBt>0.1SSB0)","P(SSBt>2SSB2011)", "SSB2025/SSB2011","AveC 2013-2025","AveC 2013-2035", "10th SSBt","10th Ct",
							"1. P(2 yr C up down)", "2. P(4 yr C up down)", "3. C wiggle", "4. P(Ctuning>C2011)","5. P(SSBtuning>B2011)", 
							"6a. 10th C2030/SSB2030", "6b. 50th C2030/SSB2030", "6c. 90th C2030/SSB2030"
							)))
	i <- 1
	results[i,1:6] <- c(1,2030,3000,"noinc", 3,2020);  i<-i+1
	results[i,1:6] <- c(1, 2035,3000,"noinc", 3,2022);  i<-i+1
	results[i,1:6] <- c(1, 2035,5000,"noinc", 3,2022);  i<-i+1
	results[i,1:6] <- c(1, 2040,3000,"noinc", 3,2025);  i<-i+1
	results[i,1:6] <- c(1, 2030,3000,"inc", 3,2020);  i<-i+1
	results[i,1:6] <- c(1, 2035,3000,"inc", 3,2022);  i<-i+1
	results[i,1:6] <- c(1, 2035,5000,"inc", 3,2022);  i<-i+1
	results[i,1:6] <- c(1, 2040,3000,"inc", 3,2025);  i<-i+1
 
   if (!is.null(exclude)) {
      results <- results[-exclude,]
   }

	label.names <- paste("MP",results[,5],"_",results[,2],"_",results[,3],"_",results[,4],sep="")

	#*******read in all relevant base files so this is only done once
	base.s4.actual.files <- list()
	for (i in 1:nfiles) {
	  base.s4.actual.files[[i]] <- read.table(base.s4file.names[i],skip=2,header=T)
	}
	base.s3.actual.files <- list()
	for (i in 1:nfiles) {
	  base.s3.actual.files[[i]] <- read.table(base.s3file.names[i],skip=2,header=T)
	}

	#********Spawning biomass in one year relative to another year, or ratios thereof
	source("TB11.table.SSB.prob.rel.mult.R")

	#P(SSBt>0.2SSB0), where ssb0=1931
	for (i in 1:nfiles) {
	  results[i,7] <- table.SSB.prob.rel.mult(s4files=base.s4.actual.files[[i]], relative.to=c(1931), yr.required=as.numeric(results[i,6]), mult=0.2,
					  B.option=c("rel.to.single"),label.names=label.names[i], filenames=F)
	}

	#P(SSB2035>0.2SSB0), where ssb0=1931
	for (i in 1:nfiles) {
	  results[i,8] <- table.SSB.prob.rel.mult(s4files=base.s4.actual.files[[i]], relative.to=c(1931), yr.required=2035, mult=0.2,
					  B.option=c("rel.to.single"),label.names=label.names[i], filenames=F)
	}

	#P(SSBt>0.1SSB0), where ssb0=1931
	for (i in 1:nfiles) {
	  results[i,9] <- table.SSB.prob.rel.mult(s4files=base.s4.actual.files[[i]], relative.to=c(1931), yr.required=as.numeric(results[i,6]), mult=0.1,
					  B.option=c("rel.to.single"),label.names=label.names[i], filenames=F)
	}

	#P(SSBt>2SSB2011)
	for (i in 1:nfiles) {
	  results[i,10] <- table.SSB.prob.rel.mult(s4files=base.s4.actual.files[[i]], relative.to=c(2011), yr.required=as.numeric(results[i,6]), mult=2,
					  B.option=c("rel.to.single"),label.names=label.names[i], filenames=F)
	}

	#**********Median ratio of biomass
	source("TB11.table.SSB.quants.rel.r")

	#median SSB2025/SSB2011
	for (i in 1:nfiles) {
	  x <- table.SSB.quants.rel(s4files=base.s4.actual.files[[i]], relative.to=2011, yr.required=2025,
					  B.option=c("rel.to.single"),label.names=label.names[i], quants=c(0.5),filenames=F)
	  results[i,11] <- round(x[[1]],4)
	}

	#**********Mean catch  2013-2025
	source("TB11.table.catch.quants.mean.r")
	#Median over grid of the mean catch over 2013-2025  (mean of each row, then median of means)
	for (i in 1:nfiles) {
		x <- table.catch.quants.mean(s3files=base.s3.actual.files[[i]], yr.start=2013, yr.end=2025, 
					  label.names=label.names[i], quants=c(0.5), filenames=F)
		results[i,12] <- round(x[[1]],0)
	}

	#**********Mean catch  2013-2035
	source("TB11.table.catch.quants.mean.r")
	#Median over grid of the mean catch over 2013-2035  (mean of each row, then median of means)
	for (i in 1:nfiles) {
		x <- table.catch.quants.mean(s3files=base.s3.actual.files[[i]], yr.start=2013, yr.end=2035, 
					  label.names=label.names[i], quants=c(0.5), filenames=F)
		results[i,13] <- round(x[[1]],0)
	}

	#**********percentile of biomass
	#10th percentile of biomass in year Bt
	for (i in 1:nfiles) {
		 tmp <- base.s4.actual.files[[i]]
		 tmp <- as.matrix(tmp[,-c(1,2)])
		 yr.interest <- as.numeric(results[i,6])
		 col.interest <- grep(paste("B.",yr.interest,sep=""),colnames(tmp))
		 results[i,14] <- round(quantile(tmp[,col.interest], probs=0.1),0)
	}

	#**********percentile of catch
	#10th percentile of catch in year Bt
	for (i in 1:nfiles) {
		 tmp <- base.s3.actual.files[[i]]
		 tmp <- as.matrix(tmp[,-c(1,2)])
		 yr.interest <- as.numeric(results[i,6])
		 col.interest <- grep(paste("C.",yr.interest,sep=""),colnames(tmp))
		 results[i,15] <- round(quantile(tmp[,col.interest], probs=0.1),0)
	}

	#*********probability of C going up and then down
	#returns both the first two change point and the first four change points
	source("TB11.prob.C.updown.v2.R")
	for (i in 1:nfiles) {
		 tmp <- base.s3.actual.files[[i]]
		 x <- prob.C.updown(s3file=tmp,xlim=c(2012,2022))
		 results[i,16:17] <- x
	}

	#*********wiggle factor: how much does catch go up and down
	source("TB11.wiggle v3.r")
	for (i in 1:nfiles) {
		 tmp <- base.s3.actual.files[[i]]
		 x <- wiggle(s3file=tmp, start.yr=2013, end.yr=2035, quants=c(0.5))
		 results[i,18] <- x
	}

	#**********probability of catch greater than another catch
	source("TB11.table.C.prob.rel.mult.R")
	#P(Ctuning>C2011)
	for (i in 1:nfiles) {
	  results[i,19] <- table.C.prob.rel.mult(s3files=base.s3.actual.files[[i]], relative.to=c(2011), yr.required=as.numeric(results[i,2]), mult=1,
					  B.option=c("rel.to.single"),label.names=label.names[i], filenames=F)
	}

	#P(SSBtuning>SSB2011)
	for (i in 1:nfiles) {
	  results[i,20] <- table.SSB.prob.rel.mult(s4files=base.s4.actual.files[[i]], relative.to=c(2011), yr.required=as.numeric(results[i,2]), mult=1,
					  B.option=c("rel.to.single"),label.names=label.names[i], filenames=F)
	}


	#**********ratio of catch to SSB
	for (i in 1:nfiles) {
		 tmp3 <- base.s3.actual.files[[i]]
		 tmp3 <- as.matrix(tmp3[,-c(1,2)])
		 tmp4 <- base.s4.actual.files[[i]]
		 tmp4 <- as.matrix(tmp4[,-c(1,2)])

		 Ccol <- grep("C.2030",colnames(tmp3))
		 Bcol <- grep("B.2030",colnames(tmp4))
		 ratio <- tmp3[,Ccol]/tmp4[,Bcol]
		 x <- round(quantile(ratio, probs=c(0.1,0.5,0.9)),4)
		 results[i,21:23] <- x
	}

   invisible(results)
}

