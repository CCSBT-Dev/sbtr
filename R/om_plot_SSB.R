#' SSB by year comparison
#'
#' @param data.objects a list of the _lab.rep files from all grid cells
#' @param lev.file has the scenarios of 2000 sampled grid cells
#' @import ggplot2
#' @export
#'
plot_SSB_compare <- function (data_objects, lev_files, scenario_names, sample = TRUE) {
    dd <- NULL
    for (j in 1:length(data_objects)) {
        dobj <- data_objects[[j]]
        xx <- dobj[[1]]
        years <- (xx$years[1] - 1):xx$years[2]
        nyears <- length(years)
        nobjects <- length(dobj)
        npar <- nchar(xx$scenario_number)
        SSB <- matrix(NA, nobjects, nyears)
        scenario <- vector(length = nobjects)
        for (i in 1:nobjects) {
            xx <- dobj[[i]]
            scenario[i] = xx$scenario_number
            SSB[i, ] <- xx$Sbio
        }
        if (sample) {
            lev <- read.table(file = lev_files[j], colClasses = "numeric", sep = " ")
            nlevs = nrow(lev)
            lev.scens <- vector(length = nobjects)
            for (i in 1:nlevs) {
                lev.scens[i] <- as.numeric(paste(lev[i, 1:npar], sep = "", collapse = ""))
            }
            resamps <- match(lev.scens, scenario)
            result <- list(scenario = lev.scens, years = years, SSB = SSB[resamps, ])
        } else {
            result <- list(scenario = lev.scens, years = years, SSB = SSB[resamps, ])
        }
        d <- data.frame(result$SSB)
        names(d) <- years
        d <- reshape2::melt(d) %>%
            dplyr::mutate(Year = as.numeric(as.character(variable))) %>%
            dplyr::mutate(Scenario = scenario_names[j])
        dd <- rbind(dd, d)
    }

    ggplot(dd, aes(x = Year, y = value, colour = factor(Scenario), fill = factor(Scenario))) +
        stat_summary(fun.ymin = function(x) quantile(x, 0.05), fun.ymax = function(x) quantile(x, 0.95), geom = "ribbon", alpha = 0.25, colour = NA) +
        stat_summary(fun.y = function(x) quantile(x, 0.5), geom = "line", lwd = 1) +
        labs(x = "Year", y = "Total reproductive output", colour = "Scenario", fill = "Scenario") +
        theme_bw()
}


#' Boxplots Harvest rates by age ranges
#'
#' @param data.objects a list of the _lab.rep files from all grid cells
#' @param lev.file has the scenarios of 2000 sampled grid cells
#' @export
#'
plot_SSB <- function(data.objects, lev.file = "base.lev", sample = TRUE)
{
    xx <- data.objects[[1]]
    years <- (xx$years[1] - 1):xx$years[2]
    nyears <- length(years)
    nobjects <- length(data.objects)
    npar <- nchar(xx$scenario_number)

    SSB <- matrix(NA, nobjects, nyears)
    scenario <- vector(length = nobjects)

    for (i in 1:nobjects)
    {
        xx <- data.objects[[i]]
        scenario[i] = xx$scenario_number
        SSB[i,] <- xx$Sbio
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
        result <- list("scenario" = lev.scens, "years" = years, "SSB" = SSB[resamps,])
    } else{
        result <- list("scenario" = lev.scens, "years" = years, "SSB" = SSB[resamps,])
    }

    d <- data.frame(result$SSB)
    names(d) <- years
    d <- reshape2::melt(d) %>%
        dplyr::mutate(Year = as.numeric(as.character(variable)))

    ggplot(d, aes(x = Year, y = value/1000, group = Year)) +
        #geom_boxplot(fill = 'purple') +
        geom_violin(colour = 'purple', fill = 'purple', scale = "width", draw_quantiles = 0.5, alpha = 0.75) +
        labs(x = 'Year', y = 'Total reproductive output') +
        theme_bw()
}
