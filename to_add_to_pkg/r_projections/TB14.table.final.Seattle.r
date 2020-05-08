#############################################################################
#Big table requested by the OMMP5 including important components.
#Created by Trevor A. Branch starting 27 June 2014 
#tbranch@gmail.com / tbranch@uw.edu
#Notes:
#Throughout the biomass referred to is B10+
#and not SSB as redefined. 
#Assumed  that B1931 = B0. 
#############################################################################

#Table: everything wanted from Seattle OMMP meeting
#All biomasses are B10+. Assumed that B0=B1931
#10th, 50th, 90th of the following:
#B2014/B0   10th, 50th, 90th
#B2025/B0   10th, 50th, 90th
#B2035/B0   10th, 50th, 90th
#P(B2025 > 0.2B0)
#P(B2035 > 0.2B0)
#Sum(catch[2014:2035])       10th, 50th, 90th
#Sum(TAC[2014:2035])         10th, 50th, 90th
#Average(catch[2014:2035])   10th, 50th, 90th
#Average(TAC[2014:2035])     10th, 50th, 90th
#median(catch[2025])
#median(catch[2035])
#median(TAC[2025])
#median(TAC[2035])
#=================================================

table.final <- function(prefixes) {
   nfiles <- length(prefixes)
	base.s4file.names <- paste(prefixes,".s4",sep="")
	base.s9file.names <- paste(prefixes,".s9",sep="")

	#Note that these can be trivially reordered if necessary
	results <- matrix(nrow=nfiles, ncol=27, 
         dimnames=list(prefixes,c("B2014/B0 10th", "B2014/B0 50th", "B2014/B0 90th",
             "B2025/B0 10th", "B2025/B0 50th", "B2025/B0 90th",
             "B2035/B0 10th", "B2035/B0 50th", "B2035/B0 90th",
             "P(B2025>0.2B0)", "P(B2035>0.2B0)",
             "sum(C14-35) 10th", "sum(C14-35) 50th", "sum(C14-35) 90th",
             "sum(TAC14-35) 10th", "sum(TAC14-35) 50th", "sum(TAC14-35) 90th",
             "ave(C14-35) 10th", "ave(C14-35) 50th", "ave(C14-35) 90th",
             "ave(TAC14-35) 10th", "ave(TAC14-35) 50th", "ave(TAC14-35) 90th",
             "medC2025", "medC2035", "medTAC2025", "medTAC2035"
         )) )

	#*******read in all relevant base files so this is only done once
   for (i in 1:nfiles) {
      tmp.s4 <- read.table(base.s4file.names[i],skip=2,header=T)
      tmp.s9 <- read.table(base.s9file.names[i],skip=1,header=T)
      #print(names(tmp.s9))
      
      #B2014/B0 10th 50th 90th
      col.required <- grep(paste("TB10plus.",2014,sep=""),colnames(tmp.s4))
      col.rel.to <- grep(paste("TB10plus.",1931,sep=""),colnames(tmp.s4))
      bworms<- tmp.s4[,col.required]/tmp.s4[,col.rel.to]
      results[i,1:3] <- quantile(bworms,probs=c(0.1,0.5,0.9))
      
      #B2025/B0 10th 50th 90th   
      col.required <- grep(paste("TB10plus.",2025,sep=""),colnames(tmp.s4))
      bworms<- tmp.s4[,col.required]/tmp.s4[,col.rel.to]
      results[i,4:6] <- quantile(bworms,probs=c(0.1,0.5,0.9))
      
      #B2035/B0 10th 50th 90th   
      col.required <- grep(paste("TB10plus.",2035,sep=""),colnames(tmp.s4))
      bworms<- tmp.s4[,col.required]/tmp.s4[,col.rel.to]
      results[i,7:9] <- quantile(bworms,probs=c(0.1,0.5,0.9))

      #P(B2025 > 0.2B0)
      col.required <- grep(paste("TB10plus.",2025,sep=""),colnames(tmp.s4))
      bworms<- tmp.s4[,col.required]/(0.2*tmp.s4[,col.rel.to])   
      results[i,10] <- sum(bworms>1)/length(bworms) 
         
      #P(B2035 > 0.2B0)
      col.required <- grep(paste("TB10plus.",2035,sep=""),colnames(tmp.s4))
      bworms<- tmp.s4[,col.required]/(0.2*tmp.s4[,col.rel.to])   
      results[i,11] <- sum(bworms>1)/length(bworms) 

      #Sum(catch[2014:2035]) 10th, 50th, 90th
      #note that because C and TAC end with C. grep returns 
      #two elements
      col.start <- grep(paste("C.",2014,sep=""),colnames(tmp.s9))[1]
      col.end <- grep(paste("C.",2035,sep=""),colnames(tmp.s9))[1]
      cworms <- rowSums(tmp.s9[,col.start:col.end])
      results[i,12:14] <- quantile(cworms,probs=c(0.1,0.5,0.9))

      #Sum(TAC[2014:2035]) 10th, 50th, 90th
      col.start <- grep(paste("TAC.",2014,sep=""),colnames(tmp.s9))
      col.end <- grep(paste("TAC.",2035,sep=""),colnames(tmp.s9))
      cpworms <- rowSums(tmp.s9[,col.start:col.end])
      results[i,15:17] <- quantile(cpworms,probs=c(0.1,0.5,0.9))
      
      #mean(catch[2014:2035]) 10th, 50th, 90th
      #note that because C and TAC end with C. grep returns 
      #two elements
      col.start <- grep(paste("C.",2014,sep=""),colnames(tmp.s9))[1]
      col.end <- grep(paste("C.",2035,sep=""),colnames(tmp.s9))[1]
      cworms <- rowMeans(tmp.s9[,col.start:col.end])
      results[i,18:20] <- quantile(cworms,probs=c(0.1,0.5,0.9))
      
      #mean(TAC[2014:2035]) 10th, 50th, 90th
      col.start <- grep(paste("TAC.",2014,sep=""),colnames(tmp.s9))
      col.end <- grep(paste("TAC.",2035,sep=""),colnames(tmp.s9))
      cpworms <- rowMeans(tmp.s9[,col.start:col.end])
      results[i,21:23] <- quantile(cpworms,probs=c(0.1,0.5,0.9))

      #med(catch[2025])
      #note that because C and TAC end with C. grep returns two elements
      col1 <- grep(paste("C.",2025,sep=""),colnames(tmp.s9))[1]
      results[i,24] <- quantile(tmp.s9[,col1],probs=c(0.5))

      #med(catch[2035])
      col2 <- grep(paste("C.",2035,sep=""),colnames(tmp.s9))[1]
      results[i,25] <- quantile(tmp.s9[,col2],probs=c(0.5))
      
      #med(TAC[2025])
      col1 <- grep(paste("TAC.",2025,sep=""),colnames(tmp.s9))
      results[i,26] <- quantile(tmp.s9[,col1],probs=c(0.5))
      
      #med(TAC[2035])
      col2 <- grep(paste("TAC.",2035,sep=""),colnames(tmp.s9))
      results[i,27] <- quantile(tmp.s9[,col2],probs=c(0.5))
   }   

   invisible(results)
}
#temp <- table.final()
