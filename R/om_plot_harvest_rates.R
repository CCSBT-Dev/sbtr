#' Boxplots harvest rates by age ranges
#'
#' @param data.objects a list of the _lab.rep files from all grid cells
#' @param lev.file has the scenarios of 2000 sampled grid cells
#' @param sample sample
#' @author Ana Parma, Darcy Webber
#' @export
#' 
plot_harvest_rates <- function(data.objects, lev.file = "base.lev", sample = TRUE, hline = 0.1)
{
    xx <- data.objects[[1]]
    years <- min(xx$Fs[,2]):max(xx$Fs[,2])
    nyears <- length(years)
    nobjects <- length(data.objects)
    npar <- nchar(xx$scenario_number)
  
    H.ages2.4 <- matrix(NA, nobjects,nyears)
    H.ages5.10 <- matrix(NA, nobjects,nyears)
    H.ages11plus <- matrix(NA, nobjects,nyears)
    scenario <- vector(length = nobjects)
    
    for (i in 1:nobjects)
    {
        xx <- data.objects[[i]]
        fishery = xx$Fs[,1]
        scenario[i] = xx$scenario_number
        # harvest rate by season
        H1 = xx$Fs[fishery ==3,4:33]+xx$Fs[fishery==4,4:33]+xx$Fs[fishery==5,4:33]+xx$Fs[fishery==6,4:33]
        H2 = xx$Fs[fishery ==1,4:33]+xx$Fs[fishery==2,4:33]
        H = 1-(1-H1)*(1-H2)
        H.ages2.4[i,] = round(rowMeans(H[,2:4]),3)    
        H.ages5.10[i,] = round(rowMeans(H[,5:10]),3)    
        H.ages11plus[i,] = round(rowMeans(H[,11:30]),3)   
    }
   
    if (sample)
    {
        lev <- read.table(file = lev.file, colClasses = "numeric", sep = " ")
        nlevs = nrow(lev)
        lev.scens <- vector(length = nobjects)
        for (i in 1:nlevs) 
        {
            lev.scens[i] <- as.numeric(paste(lev[i, 1:npar], sep = "", collapse = ""))
        }
        resamps <- match(lev.scens, scenario)
        result <- list("scenario" = lev.scens,"years"=years,"Hages2.4"=H.ages2.4[resamps,],"Hages5.10"=H.ages5.10[resamps,],"Hages11plus"=H.ages11plus[resamps,])
    } else{
        result <- list("scenario"=scenario,"years"=years,"Hages2.4"=H.ages2.4,"Hages5.10"=H.ages5.10,"Hages11plus"=H.ages11plus)
    }

    d1 <- data.frame(result$Hages2.4, age = "Ages 2-4")
    d2 <- data.frame(result$Hages5.10, age = "Ages 5-10")
    d3 <- data.frame(result$Hages11plus, age = "Ages 11+")
    d <- rbind(d1, d2, d3)
    names(d) <- c(result$years, "age")
    dd <- tidyr::gather(d, key = Year, value = HR, -age) %>%
        dplyr::mutate(Year = as.numeric(Year))

    ggplot(dd, aes(Year, HR, group = Year)) +
        geom_hline(yintercept = hline, linetype = "dashed") +
        facet_wrap(~age, ncol = 1, scales = "free_y") +
        geom_boxplot(fill = "pink") +
        theme_bw() +
        labs(y = "Harvest rates") +
        scale_x_continuous(minor_breaks = seq(0, 1e6, 1), breaks = seq(0, 1e6, 5))
}
