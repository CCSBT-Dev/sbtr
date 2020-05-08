#' @title Plot of 2011 and 2014 assessment results.
#' @description
#' Originally for Fig 3 of CCSBT-ESC/1409/38.
#'
#' @param previous_s4 name of the s4 file of 2011 assessment
#' @param current_s4 name of the s4 file of 2014 assessment
#' @return a plot
#' @examples
#' Traj_old_and_new_f(previous_s4="MP3_2035_3000_inc_base.s4", current_s4="MP3_v21_base.s4")
#' @export
#'
Traj_old_and_new_f <- function(previous_s4="MP3_2035_3000_inc_base.s4", current_s4="MP3_v21_base.s4", prefix="2014_assess")
{
	## previous stock assessment result ##
	previous_s4 <- previous_s4

	tmp1 <- read.table(file =previous_s4, skip=2, head=T)
	head(tmp1,5)
	colnames(tmp1)

	hist_start_yr <- 1931
	hist_end_yr   <- 2011
	prj_start_yr  <- 2012
	prj_end_yr    <- 2012+30
	tot_yr        <- prj_end_yr-hist_start_yr+1 
	nhist_yr      <- hist_end_yr-hist_start_yr+1
	nprj_yr       <- prj_end_yr- hist_end_yr

	# Previous assess Biomass #
	tmp2 <- tmp1[,3:(2+tot_yr)]
	med <- NULL
	q05 <- NULL
	q95 <- NULL
	for(i in 1:(length(tmp2[1,]))) 
	{
		tmp2med <- median(tmp2[,i])
		med     <- append(med, tmp2med)
		tmp2q05 <- quantile(tmp2[,i],0.05)
		q05     <- append(q05, tmp2q05)
		tmp2q95 <- quantile(tmp2[,i],0.95)
		q95     <- append(q95, tmp2q95)
	}
	tmp3 <- NULL
	tmp3 <- cbind(med,q05)
	tmp3 <- cbind(tmp3,q95)
	rownames(tmp3) <- colnames(tmp2)
	Pre_Bio <- tmp3

	# Previous assess Recruitment #
	tmp2 <- tmp1[,(3+tot_yr):(2+tot_yr*2)]
	med  <- NULL
	q05  <- NULL
	q95  <- NULL
	for(i in 1:(length(tmp2[1,]))) 
	{
		tmp2med <- median(tmp2[,i])
		med     <- append(med, tmp2med)
		tmp2q05 <- quantile(tmp2[,i],0.05)
		q05     <- append(q05, tmp2q05)
		tmp2q95 <- quantile(tmp2[,i],0.95)
		q95     <- append(q95, tmp2q95)
	}
	tmp3 <- NULL
	tmp3 <- cbind(med,q05)
	tmp3 <- cbind(tmp3,q95)
	rownames(tmp3) <- colnames(tmp2)
	Pre_Rec <- tmp3

	## Newest assess result ##
	current_s4 <- current_s4
	#current_s4 <- "projections\\Result\\20140811mp3\\MP3_v21_base.s4"
	# New assess Biomass #
	tmp4 <- read.table(file=current_s4, skip=2, head=T)
	head(tmp4,5)
	colnames(tmp4)

	hist_start_yr <- 1931
	hist_end_yr <- 2013
	prj_start_yr <- 2014
	prj_end_yr <- 2014+27-1
	tot_yr <- prj_end_yr-hist_start_yr+1 
	nhist_yr <- hist_end_yr-hist_start_yr+1
	nprj_yr <- prj_end_yr- hist_end_yr

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
	New_Bio <- tmp6

	# Newest assess Sp_Bio_index #
	tmp1 <- read.table(file=current_s4, skip=2, head=T)
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
	New_SSBindex <- tmp3

	# New assess Recruitment #
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
	New_Rec <- tmp3


	#### Plot updated "base case" and previous one ########################################################################

	filename <- "Assessment_Old_and_New"
	outputf <- paste("Figs\\", prefix, "_Traj_", filename, ".pdf", sep="")
	pdf(outputf, width=7, height=10, paper="A4")
	par(mfrow=c(3,1), mai=c(0.3,1,0.3,.2), omi=c(0.5,0,0.6,0))


	## RECRUIT ##
	dnum <- length(New_Rec[,1])
	plot(Pre_Rec[,3], type="n", xlab="Year", ylab="Recruit (N)", ylim=c(0, max(Pre_Rec[,3])-2), axes=F)

	# old_hist
	polygon(c(1:(nhist_yr-3),(nhist_yr-3):1), c(Pre_Rec[1:(nhist_yr-3),2],Pre_Rec[(nhist_yr-3):1,3]),border=F, col="#0000FF40")	
	# old_proj
	polygon(c((nhist_yr-3):(tot_yr),(tot_yr):(nhist_yr-3)), c(Pre_Rec[(nhist_yr-3):(tot_yr),2],Pre_Rec[(tot_yr):(nhist_yr-3),3]),border=F, col="#00BFFF40")	
	# old
	points(c(1:(dnum)), Pre_Rec[1:(dnum),1], type="l", lwd=2, col="#4682B4")
	abline(v=(nhist_yr-3), col="#0000FF", lty=3, lwd=2)

	# new_hist(with CK)
	polygon(c(1:(nhist_yr),(nhist_yr):1), c(New_Rec[1:(nhist_yr),2],New_Rec[(nhist_yr):1,3]),border=F, col="#FF149390")	
	# new_proj(with CK)
	polygon(c((nhist_yr):tot_yr,tot_yr:(nhist_yr)),c(New_Rec[(nhist_yr):tot_yr,2],New_Rec[tot_yr:(nhist_yr),3]),border=F, col="#FFC0CB90")	
	# new(with CK)
	points(c(1:dnum), New_Rec[1:dnum,1], type="l", lwd=2, col="red")
	abline(v=(nhist_yr), col="#FF1493", lty=3, lwd=2)

	# axis
	axis(1, pos=NA, at=seq(0,dnum,10), labels=1931+seq(0,dnum,10)-1, adj=0, cex.axis=0.8, cex.lab=0.9)
	axis(2, pos=NA, at=pretty(par("usr")[3:4]), adj=1, cex.axis=0.8, cex.lab=0.9)

	# legend
	legend("topright", c("2011 assess","2014 base case"), lwd=2, 
	col=c("#4682B4", "red"), bg="white")

	# legend
	legend("bottomleft", c("a)"), box.col="white", cex=2,
	bg="white", x.intersp=0)

	box()


	## BIOMASS (age10+) ##
	dnum <- length(New_Bio[,1])
	plot(New_Bio[,3], type="n", xlab="Year", ylim=c(0, max(New_Bio[,3])), ylab="Biomass(t) of age10+", axes=F)

	# old_hist
	polygon(c(1:(nhist_yr-3),(nhist_yr-3):1), c(Pre_Bio[1:(nhist_yr-3),2],Pre_Bio[(nhist_yr-3):1,3]),border=F, col="#0000FF40")	
	# old_proj
	polygon(c((nhist_yr-3):(tot_yr),(tot_yr):(nhist_yr-3)), c(Pre_Bio[(nhist_yr-3):(tot_yr),2],Pre_Bio[(tot_yr):(nhist_yr-3),3]),border=F, col="#00BFFF40")
	# old
	points(c(1:(dnum)), Pre_Bio[1:(dnum),1], type="l", lwd=2, col="#4682B4")
	abline(v=(nhist_yr-3), col="#0000FF", lty=3, lwd=2)

	# new_hist(with CK)
	polygon(c(1:(nhist_yr),(nhist_yr):1), c(New_Bio[1:(nhist_yr),2],New_Bio[(nhist_yr):1,3]),border=F, col="#FF149390")	
	# new_proj(with CK)
	polygon(c((nhist_yr):tot_yr,tot_yr:(nhist_yr)),c(New_Bio[(nhist_yr):tot_yr,2],New_Bio[tot_yr:(nhist_yr),3]),border=F, col="#FFC0CB90")	
	points(c(1:dnum), New_Bio[1:dnum,1], type="l", lwd=2, col="red")
	abline(v=(nhist_yr), col="#FF1493", lty=3, lwd=2)

	# axis
	axis(1, pos=NA, at=seq(0,dnum,10), labels=1931+seq(0,dnum,10)-1, adj=0, cex.axis=0.8, cex.lab=0.9)
	axis(2, pos=NA, at=pretty(par("usr")[3:4]), adj=1, cex.axis=0.8, cex.lab=0.9)

	# legend
	legend("topright", c("2011 assess","2014 base case"), lwd=2, 
	col=c("#4682B4", "red"), bg="white")

	# legend
	legend("bottomleft", c("b)"), box.col="white", cex=2,
	bg="white", x.intersp=0)

	box()


	## SSB index ##
	dnum <- length(New_SSBindex[,1])
	plot(New_SSBindex[,3], type="n", xlab="Year", ylim=c(0, max(New_SSBindex[,3])), ylab="SSB Index", axes=F)

	# new_hist(with CK)
	polygon(c(1:(nhist_yr),(nhist_yr):1), c(New_SSBindex[1:(nhist_yr),2],New_SSBindex[(nhist_yr):1,3]),border=F, col="#FF149390")	
	# new_proj(with CK)
	polygon(c((nhist_yr):tot_yr,tot_yr:(nhist_yr)),c(New_SSBindex[(nhist_yr):tot_yr,2],New_SSBindex[tot_yr:(nhist_yr),3]),border=F, col="#FFC0CB90")	
	# new(with CK)
	points(c(1:dnum), New_SSBindex[1:dnum,1], type="l", lwd=2, col="red")
	abline(v=(nhist_yr), col="#FF1493", lty=3, lwd=2)

	# axis
	axis(1, pos=NA, at=seq(0,dnum,10), labels=1931+seq(0,dnum,10)-1, adj=0, cex.axis=0.8, cex.lab=0.9)
	axis(2, pos=NA, at=pretty(par("usr")[3:4]), adj=1, cex.axis=0.8, cex.lab=0.9)

	# legend
	legend("topright", c("2014 base case"), lwd=2, 
	col=c("red"), bg="white")

	# legend
	legend("bottomleft", c("c)"), box.col="white", cex=2,
	bg="white", x.intersp=0)

	box()

	dev.off()

}
