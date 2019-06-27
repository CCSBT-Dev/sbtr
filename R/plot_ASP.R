#' Plot ASP
#'
#' data.objects is the collection of lab.rep files
#' @export
#'
plot_ASP <- function(data.objects, lev.file = "base.lev", a1 = 2, a2 = 15, plot.yr1 = 1955) 
{
    xx <- data.objects[[1]]
    years <- xx$yrs.catch[1]:xx$yrs.catch[2] 
    nyears <- length(years)
    nobjects <- length(data.objects)
    allyears <- xx$years[1]:xx$years[2]
    nallyears <- length(allyears)
    TOTcatch <- rep(0, nallyears)
    TOTcatch[allyears >= xx$yrs.catch[1]] = apply(xx$catch.weight.pred, 2, sum)
    
    CSProd <- matrix(NA, nobjects, nallyears)  
    scenario <- vector(length = nobjects)
    
    for (i in 1:nobjects)
    {
        xx <- data.objects[[i]]
        scenario[i] <- xx$scenario_number
        TOTbio <- xx$TOTbio
        CSProd[i,] <- TOTcatch + diff(TOTbio)      
    }       
    
    lev <- read.table(file = lev.file, colClasses = "numeric", sep = " ")
    nlevs <- nrow(lev)
    lev.scens <- as.numeric(apply(lev[,1:7], 1, function(x) { paste(x, collapse = "") }))

    resamps <- match(lev.scens, scenario)
    result <- CSProd[resamps,]
    id.years <- allyears >= plot.yr1

    d <- data.frame(result[,id.years])
    names(d) <- allyears[id.years]
    d <- tidyr::gather(d, key = Year) %>%
        dplyr::mutate(Year = as.numeric(Year))

    ggplot(d, aes(Year, value, group = Year)) +
        geom_boxplot(fill = "green") +
        theme_bw() +
        labs(y = "Surplus production") +
        scale_x_continuous(breaks = seq(0, 1e6, 10), minor_breaks = seq(0, 1e6, 1))
}


#' Plot ASP projections
#'
#' data.objects is the collection of lab.rep files
#' @export
#'
plot_ASP_projection <- function(data.objects, lev.file = "base.lev", s3filenames = ".s10", plot.yr1 = 1955) 
{
    xx <- data.objects[[1]]
    years <- xx$yrs.catch[1]:xx$yrs.catch[2] 
    nyears <- length(years)
    nobjects <- length(data.objects)
    allyears <- xx$years[1]:xx$years[2]
    nallyears <- length(allyears)
    TOTcatch <- rep(0, nallyears)
    TOTcatch[allyears >= xx$yrs.catch[1]] <- apply(xx$catch.weight.pred, 2, sum)
    
    sprod1 <- matrix(NA, nobjects, nallyears)
    scatch1 <- matrix(NA, nobjects, nallyears)
    sbio1 <- matrix(NA, nobjects, nallyears)
    scenario <- vector(length = nobjects)
    
    for (i in 1:nobjects)
    {
        xx <- data.objects[[i]]
        scenario[i] <- xx$scenario_number
        TOTbio <- xx$TOTbio
        sprod1[i,] <- TOTcatch + diff(TOTbio)
        sbio1[i,] <- TOTbio[2:length(TOTbio)]
        scatch1[i,] <- TOTcatch
    }
    
    lev <- read.table(file = lev.file, colClasses = "numeric", sep = " ")
    nlevs <- nrow(lev)
    lev.scens <- as.numeric(apply(lev[,1:7], 1, function(x) { paste(x, collapse = "") }))
    resamps1 <- match(lev.scens, scenario)

    s3file <- read.table(s3filenames, skip = 2, header = TRUE)
    c.cols <- grep("C.", colnames(s3file))
    b.cols <- grep("B.", colnames(s3file))
    
    lev.scens <- as.numeric(apply(lev[,1:8], 1, function(x) { paste(x, collapse = "") }))
    resamps2 <- match(lev.scens, s3file$scenario)
    
    scatch2 <- s3file[, c.cols]
    sbio2 <- s3file[, b.cols]
    sprod2 <- scatch2 # gets over-written in following line
    for (i in 1:nrow(catches)) sprod2[i,] <- scatch2[i,] + diff(as.numeric(sbio2[i,]))

    d1 <- data.frame(sprod1[resamps1,])
    names(d1) <- allyears
    d1a <- tidyr::gather(d1, key = Year) %>%
        dplyr::mutate(Year = as.numeric(Year), Proj = 0, Key = "Surplus production") %>%
        dplyr::filter(Year >= plot.yr1)
    d2a <- tidyr::gather(sprod2, key = yr, value = value) %>%
        dplyr::mutate(Year = as.numeric(gsub("\\D", "", yr))) %>%
        dplyr::select(Year, value) %>%
        dplyr::mutate(Proj = 1, Key = "Surplus production", value = value) %>%
        dplyr::filter(Year >= plot.yr1)

    d1 <- data.frame(sbio1[resamps1,])
    names(d1) <- allyears
    d1b <- tidyr::gather(d1, key = Year) %>%
        dplyr::mutate(Year = as.numeric(Year), Proj = 0, Key = "Total biomass") %>%
        dplyr::filter(Year >= plot.yr1)
    d2b <- tidyr::gather(sbio2, key = yr, value = value) %>%
        dplyr::mutate(Year = as.numeric(gsub("\\D", "", yr))) %>%
        dplyr::select(Year, value) %>%
        dplyr::mutate(Proj = 1, Key = "Total biomass", value = value) %>%
        dplyr::filter(Year >= plot.yr1)

    d1 <- data.frame(scatch1[resamps1,])
    names(d1) <- allyears
    d1c <- tidyr::gather(d1, key = Year) %>%
        dplyr::mutate(Year = as.numeric(Year), Proj = 0, Key = "Catch") %>%
        dplyr::filter(Year >= plot.yr1)
    d2c <- tidyr::gather(scatch2, key = yr, value = value) %>%
        dplyr::mutate(Year = as.numeric(gsub("\\D", "", yr))) %>%
        dplyr::select(Year, value) %>%
        dplyr::mutate(Proj = 1, Key = "Catch", value = value) %>%
        dplyr::filter(Year >= plot.yr1)

    d <- rbind(d1a, d2a, d1b, d2b, d1c, d2c)
    d$Key <- factor(d$Key, levels = c("Total biomass","Catch","Surplus production"))
    head(d)

    ggplot(d, aes(Year, value, group = Year)) +
        geom_violin(aes(colour = as.factor(Proj), fill = as.factor(Proj)), scale = "width", alpha = 0.75, draw_quantiles = 0.5) +
        theme_bw() +
        facet_wrap(~Key, ncol = 1, scales = "free_y") +
        labs(y = NULL) +
        scale_x_continuous(breaks = seq(0, 1e6, 10), minor_breaks = seq(0, 1e6, 1)) +
        theme(legend.position = "none")
}
