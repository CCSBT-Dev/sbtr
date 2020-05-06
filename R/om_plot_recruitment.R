#' Plot of recruitment
#'
#' @param data.objects a list of the _lab.rep files from all grid cells
#' @param lev.file has the scenarios of 2000 sampled grid cells
#' @param scenario_name a vector of name labels for the scenarios being plotted
#' @export
#'
plot_recruitment <- function (data.objects, lev.file = "base.lev", scenario_name, sample = TRUE) {
  dd <- NULL
  for (j in 1:length(data.objects)) {
    dobj <- data.objects[[j]]
    xx <- dobj[[1]]
    years <- (xx$years[1] - 1):xx$years[2]
    nyears <- length(years)
    nobjects <- length(dobj)
    npar <- nchar(xx$scenario_number)
    SSB <- matrix(NA, nobjects, nyears)
    scenario <- vector(length = nobjects)
    for (i in 1:nobjects) {
      xx <- dobj[[i]]
      scenario[i] <- xx$scenario_number
      SSB[i, ] <- xx$Recruitment
    }
    if (sample) {
      lev <- read.table(file = lev.file[j], colClasses = "numeric", sep = " ")
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
      mutate(Year = as.numeric(as.character(variable))) %>%
      mutate(Scenario = scenario_name[j])
    dd <- rbind(dd, d)
  }

  ggplot(dd, aes(x = Year, y = value, colour = factor(Scenario), fill = factor(Scenario))) +
    stat_summary(fun.min = function(x) quantile(x, 0.05), fun.max = function(x) quantile(x, 0.95), geom = "ribbon", alpha = 0.25, colour = NA) +
    stat_summary(fun.y = function(x) quantile(x, 0.5), geom = "line", lwd = 1) +
    scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.05))) +
    labs(x = "Year", y = "Recruitment", colour = "Scenario", fill = "Scenario") +
    theme_bw()
}
