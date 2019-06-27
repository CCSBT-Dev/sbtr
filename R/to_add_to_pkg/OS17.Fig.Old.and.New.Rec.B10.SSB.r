#' @title Plot of current and previous assessment results.
#' @description
#'
#' @param previous_s4 name of the s4 file of previous assessment
#' @param current_s4 name of the s4 file of current assessment
#' @return a plot
#' @examples
#' Traj_old_and_new_f(previous_s4="MP3_2035_3000_inc_base.s4", prev_hist_end=2013, prev_prj_end=2041, regendprev="2014 base", current_s4="MP3_v21_base.s4", curr_hist_end=2016, curr_prj_end=2041, regendcurr="2017 base", prefix="2011vs2014", pdf=F)
#' @export
#'
Traj_old_and_new_f <- function(previous_s4="MP3_2035_3000_inc_base.s4", prev_hist_end=2013, prev_prj_end=2041, regendprev="2011 assess",
								current_s4="MP3_v21_base.s4", curr_hist_end=2016, curr_prj_end=2041, regendcurr="2014 assess",
								prefix="2011vs2014", pdf=F, outtext=F)
{
	hist_start_yr <- 1931
 
    datainput <- function(input_s4=input_s4, hist_start_yr=hist_start_yr, hist_end_yr=hist_end_yr, prj_end_yr=prj_end_yr)
    {
		prj_start_yr  <- hist_end_yr+1
		tot_yr        <- prj_end_yr-hist_start_yr+1 
		nhist_yr      <- hist_end_yr-hist_start_yr+1
		nprj_yr       <- prj_end_yr- hist_end_yr
		
		tmp1 <- read.table(file =input_s4, skip=2, head=T)
	
		#  Total Reproductive Output (Sp_Bio) #
		tmp2 <- tmp1[,3:(2+tot_yr)]
		Tro_med <- NULL
		Tro_q05 <- NULL
		Tro_q95 <- NULL
		for(i in 1:(length(tmp2[1,]))) 
		{
			tmp2med <- median(tmp2[,i])
			Tro_med <- append(Tro_med, tmp2med)
			tmp2q05 <- quantile(tmp2[,i],0.05)
			Tro_q05 <- append(Tro_q05, tmp2q05)
			tmp2q95 <- quantile(tmp2[,i],0.95)
			Tro_q95 <- append(Tro_q95, tmp2q95)
		}
		data <- NULL
		data <- cbind(Tro_med,Tro_q05)
		data <- cbind(data,Tro_q95)
		rownames(data) <- c(1:length(data[,1]))
		data <- data.frame(data)
		data$year <- c(hist_start_yr: (hist_start_yr+length(data[,1])-1))

		# Recruitment #
		tmp2 <- tmp1[,(3+tot_yr):(2+tot_yr*2)]
		Rec_med  <- NULL
		Rec_q05  <- NULL
		Rec_q95  <- NULL
		for(i in 1:(length(tmp2[1,]))) 
		{
			tmp2med <- median(tmp2[,i])
			Rec_med <- append(Rec_med, tmp2med)
			tmp2q05 <- quantile(tmp2[,i],0.05)
			Rec_q05 <- append(Rec_q05, tmp2q05)
			tmp2q95 <- quantile(tmp2[,i],0.95)
			Rec_q95 <- append(Rec_q95, tmp2q95)
		}
		tmp3 <- NULL
		tmp3 <- cbind(Rec_med,Rec_q05)
		tmp3 <- cbind(tmp3,Rec_q95)
		rownames(tmp3) <- c(1:length(tmp3[,1]))
		tmp3 <- data.frame(tmp3)
		tmp3$year <- c(hist_start_yr: (hist_start_yr+length(tmp3[,1])-1))
		data <- merge(data,tmp3, by=("year"), all.x=T)

		# Biomass age10+ #
		tmp2 <- tmp1[,(3+tot_yr*2):(1+tot_yr*3)]
		Bio10_med <- NULL
		Bio10_q05 <- NULL
		Bio10_q95 <- NULL
		for(i in 1:(length(tmp2[1,]))) 
		{
			tmp2med <- median(tmp2[,i])
			Bio10_med <- append(Bio10_med, tmp2med)
			tmp2q05 <- quantile(tmp2[,i],0.05)
			Bio10_q05 <- append(Bio10_q05, tmp2q05)
			tmp2q95 <- quantile(tmp2[,i],0.95)
			Bio10_q95 <- append(Bio10_q95, tmp2q95)
		}
		tmp3 <- NULL
		tmp3 <- cbind(Bio10_med,Bio10_q05)
		tmp3 <- cbind(tmp3,Bio10_q95)
		rownames(tmp3) <- c(1:length(tmp3[,1]))
		tmp3 <- data.frame(tmp3)
		tmp3$year <- c(hist_start_yr: (hist_start_yr+length(tmp3[,1])-1))
		data <- merge(data,tmp3, by=("year"), all.x=T)

		data
	}

	## previous stock assessment result ##
	input_s4 <- previous_s4
    prev_data <- datainput(input_s4=input_s4, hist_start_yr=hist_start_yr, hist_end_yr=prev_hist_end, prj_end_yr=prev_prj_end)

	## current stock assessment result ##
	input_s4 <- current_s4
    curr_data <- datainput(input_s4=input_s4, hist_start_yr=hist_start_yr, hist_end_yr=curr_hist_end, prj_end_yr=curr_prj_end)


	#### Plot updated "base case" and previous one ########################################################################

	if(pdf)
	{
		filename <- "Assessment_Old_and_New"
		outputf <- paste(prefix, "_Traj_", filename, ".pdf", sep="")
		pdf(outputf, width=7, height=10, paper="A4")
	}
	par(mfrow=c(3,1), mai=c(0.3,1,0.3,.2), omi=c(0.5,0,0.6,0))


	dnum          <- curr_prj_end - hist_start_yr+1 

	## RECRUIT ##
	Pre_Rec <- data.frame(prev_data$Rec_med, prev_data$Rec_q05, prev_data$Rec_q95)
	New_Rec <- data.frame(curr_data$Rec_med, curr_data$Rec_q05, curr_data$Rec_q95)

    # Out text #
    if(outtext)
    {
      write.csv(Pre_Rec, "Csv_pre_recruitment_poly.csv")
      write.csv(New_Rec, "Csv_new_recruitment_poly.csv")
    }
    
    # Plot #
	plot(New_Rec[,3], type="n", xlab="Year", ylab="Recruitment (N)", xlim=c(0, dnum), ylim=c(0, max(New_Rec[,3])), axes=F)

	nhist_yr      <- prev_hist_end - hist_start_yr+1
	tot_yr        <- prev_prj_end - hist_start_yr+1 
	# old_hist
	polygon(c(1:(nhist_yr),(nhist_yr):1), c(Pre_Rec[1:(nhist_yr),2],Pre_Rec[(nhist_yr):1,3]),border=F, col="#0000FF40")	
	# old_proj
	polygon(c((nhist_yr):(tot_yr),(tot_yr):(nhist_yr)), c(Pre_Rec[(nhist_yr):(tot_yr),2],Pre_Rec[(tot_yr):(nhist_yr),3]),border=F, col="#00BFFF40")	
	# old
	points(c(1:(tot_yr)), Pre_Rec[1:(tot_yr),1], type="l", lwd=2, col="#4682B4")
	abline(v=(nhist_yr), col="#0000FF", lty=3, lwd=2)

	nhist_yr      <- curr_hist_end - hist_start_yr+1
	tot_yr        <- curr_prj_end - hist_start_yr+1 
	# new_hist
	polygon(c(1:(nhist_yr),(nhist_yr):1), c(New_Rec[1:(nhist_yr),2],New_Rec[(nhist_yr):1,3]),border=F, col="#FF149390")	
	# new_proj
	polygon(c((nhist_yr):tot_yr,tot_yr:(nhist_yr)),c(New_Rec[(nhist_yr):tot_yr,2],New_Rec[tot_yr:(nhist_yr),3]),border=F, col="#FFC0CB90")	
	# new
	points(c(1:tot_yr), New_Rec[1:tot_yr,1], type="l", lwd=2, col="red")
	abline(v=(nhist_yr), col="#FF1493", lty=3, lwd=2)

	# axis
	axis(1, pos=NA, at=seq(0,dnum,10), labels=hist_start_yr+seq(0,dnum,10)-1, adj=0, cex.axis=0.8, cex.lab=0.9)
	axis(2, pos=NA, at=pretty(par("usr")[3:4]), adj=1, cex.axis=0.8, cex.lab=0.9)

	# legend
	legend("topright", c(regendprev,  regendcurr), lwd=2, 
	col=c("#4682B4", "red"), bg="white")

	# legend
	legend("bottomleft", c("a)"), box.col=NA, cex=2,
	bg=NA, x.intersp=0)

	box()

	## BIOMASS (age10+) ##
	Pre_Bio <- data.frame(prev_data$Bio10_med, prev_data$Bio10_q05, prev_data$Bio10_q95)
	Pre_Bio <- Pre_Bio[1:(length(Pre_Bio[,1])-1),]
	New_Bio <- data.frame(curr_data$Bio10_med, curr_data$Bio10_q05, curr_data$Bio10_q95)
	New_Bio <- New_Bio[1:(length(New_Bio[,1])-1),]

    # Out text #
    if(outtext)
    {
      write.csv(Pre_Bio, "Csv_pre_bio10p_poly.csv")
      write.csv(New_Bio, "Csv_new_bio10p_poly.csv")
    }
    
    # Plot #
	plot(New_Bio[,3], type="n", xlab="Year", ylim=c(0, max(New_Bio[,3])), xlim=c(0, dnum), ylab="Biomass(t) of age10+", axes=F)

	nhist_yr      <- prev_hist_end - hist_start_yr+1
	tot_yr        <- prev_prj_end - hist_start_yr 
	# old_hist
	polygon(c(1:(nhist_yr),(nhist_yr):1), c(Pre_Bio[1:(nhist_yr),2],Pre_Bio[(nhist_yr):1,3]),border=F, col="#0000FF40")	
	# old_proj
	polygon(c((nhist_yr):(tot_yr),(tot_yr):(nhist_yr)), c(Pre_Bio[(nhist_yr):(tot_yr),2],Pre_Bio[(tot_yr):(nhist_yr),3]),border=F, col="#00BFFF40")
	# old
	points(c(1:(tot_yr)), Pre_Bio[1:(tot_yr),1], type="l", lwd=2, col="#4682B4")
	abline(v=(nhist_yr), col="#0000FF", lty=3, lwd=2)

	nhist_yr      <- curr_hist_end - hist_start_yr+1
	tot_yr        <- curr_prj_end - hist_start_yr 
	# new_hist
	polygon(c(1:(nhist_yr),(nhist_yr):1), c(New_Bio[1:(nhist_yr),2],New_Bio[(nhist_yr):1,3]),border=F, col="#FF149390")	
	# new_proj
	polygon(c((nhist_yr):tot_yr,tot_yr:(nhist_yr)),c(New_Bio[(nhist_yr):tot_yr,2],New_Bio[tot_yr:(nhist_yr),3]),border=F, col="#FFC0CB90")	
	# new
	points(c(1:tot_yr), New_Bio[1:tot_yr,1], type="l", lwd=2, col="red")
	abline(v=(nhist_yr), col="#FF1493", lty=3, lwd=2)

	# axis
	axis(1, pos=NA, at=seq(0,dnum,10), labels=1931+seq(0,dnum,10)-1, adj=0, cex.axis=0.8, cex.lab=0.9)
	axis(2, pos=NA, at=pretty(par("usr")[3:4]), adj=1, cex.axis=0.8, cex.lab=0.9)

	# legend
	legend("topright", c(regendprev,  regendcurr), lwd=2, 
	col=c("#4682B4", "red"), bg="white")

	# legend
	legend("bottomleft", c("b)"), box.col="white", cex=2,
	bg="white", x.intersp=0)

	box()

	## Total Reproductive Output TRO (SSB index) ##
	Pre_SSB <- data.frame(prev_data$Tro_med, prev_data$Tro_q05, prev_data$Tro_q95)
	New_SSB <- data.frame(curr_data$Tro_med, curr_data$Tro_q05, curr_data$Tro_q95)

    # Out text #
    if(outtext)
    {
      write.csv(Pre_SSB, "Csv_pre_tro_poly.csv")
      write.csv(New_SSB, "Csv_new_tro_poly.csv")
    }
    
    # Plot #
	plot(New_SSB[,3], type="n", xlab="Year", xlim=c(0, dnum), ylim=c(0, max(New_SSB[,3])), ylab="TRO", axes=F)

	nhist_yr      <- prev_hist_end - hist_start_yr+1
	tot_yr        <- prev_prj_end - hist_start_yr+1 
	# old_hist
	polygon(c(1:(nhist_yr),(nhist_yr):1), c(Pre_SSB[1:(nhist_yr),2],Pre_SSB[(nhist_yr):1,3]),border=F, col="#0000FF40")	
	# old_proj
	polygon(c((nhist_yr):(tot_yr),(tot_yr):(nhist_yr)), c(Pre_SSB[(nhist_yr):(tot_yr),2],Pre_SSB[(tot_yr):(nhist_yr),3]),border=F, col="#00BFFF40")
	# old
	points(c(1:(tot_yr)), Pre_SSB[1:(tot_yr),1], type="l", lwd=2, col="#4682B4")
	abline(v=(nhist_yr), col="#0000FF", lty=3, lwd=2)

	nhist_yr      <- curr_hist_end - hist_start_yr+1
	tot_yr        <- curr_prj_end - hist_start_yr+1 
	# new_hist
	polygon(c(1:(nhist_yr),(nhist_yr):1), c(New_SSB[1:(nhist_yr),2],New_SSB[(nhist_yr):1,3]),border=F, col="#FF149390")	
	# new_proj
	polygon(c((nhist_yr):tot_yr,tot_yr:(nhist_yr)),c(New_SSB[(nhist_yr):tot_yr,2],New_SSB[tot_yr:(nhist_yr),3]),border=F, col="#FFC0CB90")	
	# new
	points(c(1:tot_yr), New_SSB[1:tot_yr,1], type="l", lwd=2, col="red")
	abline(v=(nhist_yr), col="#FF1493", lty=3, lwd=2)

	# axis
	axis(1, pos=NA, at=seq(0,dnum,10), labels=hist_start_yr+seq(0,dnum,10)-1, adj=0, cex.axis=0.8, cex.lab=0.9)
	axis(2, pos=NA, at=pretty(par("usr")[3:4]), adj=1, cex.axis=0.8, cex.lab=0.9)

	# legend
	legend("topright", c(regendprev,  regendcurr), lwd=2, 
	col=c("#4682B4", "red"), bg="white")

	# legend
	legend("bottomleft", c("c)"), box.col=NA, cex=2,
	bg=NA, x.intersp=0)

	box()

	if(pdf)
	{
		dev.off()
	}
}
