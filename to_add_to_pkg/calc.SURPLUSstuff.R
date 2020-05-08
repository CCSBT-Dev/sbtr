#' Surplus.stats <- calc.SURPLUSstuff.R(data.objects=base, lev.file="base2016sqrt.lev")
#' 
#' calculates surpulus production. 
#' The surpuls production is estimated as follows; 
#' catch in year t + biomass difference in year t from year t-1.
#' data.objects is the collection of lab.rep files produced by get.all.data.R()
#' Osamu Sakai, 2017  (modified from calc.SURPLUSstuff.R) 
#' 
#' @examples
#' Surplus.stats <- calc.SURPLUSstuff.R(base, base2016sqrt.lev)
#' @export
#'
calc.SURPLUSstuff.R <- function(data.objects, lev.file) {
   nobjects <- length(data.objects)
   scenario <- NULL
   CatchWt <- NULL
   Surplus <- NULL
   for (i in 1:nobjects) {
     xx <- data.objects[[i]]
     scenario <- rbind(scenario, xx$scenario_number)
     tmp1 <- xx$catch.weight.pred
     tmp2 <- NULL
     for(i in 1:length(tmp1[1,])){
         tmp2 <- append(tmp2,sum(tmp1[,i]))
     }
     tmp3 <- xx$TOTbio
     tmp4 <- tmp3[(length(tmp3)-length(tmp2)+1):length(tmp3)]
     years <- c((xx$years[2]-length(tmp4)+1):xx$years[2])
     tmp5 <- NULL
     for(i in 2:length(tmp4)){
         tmp5[i] <- tmp4[i]-tmp4[i-1]+tmp2[i]
     }
     tmp5
     Surplus <- rbind(Surplus, tmp5)
     colnames(Surplus) <- years
   }
   Surplus <- as.data.frame(Surplus)
   Surplus$scenario <- as.numeric(scenario)
   #summary(Surplus)

   resample <- read.table(lev.file)
   resample$scenario <- resample[,1]*1000000 + resample[,2]*100000 + resample[,3]*10000 + resample[,4]*1000 + resample[,5]*100 + resample[,6]*10 + resample[,7]*1

   tmp6 <- merge(resample, Surplus, by="scenario", all.x=T)
   Surplus_data <- tmp6[,11:length(tmp6[1,])]
   #head(Surplus_data)
}
