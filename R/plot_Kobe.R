#' Plots U/Umsy vs B/Bmsy over time with confidence intervals.
#'
#' Umsy and Bmsy were calculated by msycalc.tpl using anual weights, selectivities and allocation
#' Ana Parma, Bali 2011
#'
#' @param MSY.stats MSY stats outputs
#' @author Ana Parma, Darcy Webber
#' @export
#'
plot_Kobe_TRO <- function(MSY.stats) {
  Bquantiles <- MSY.stats$B.Bmsy.quantiles
  BBmsy <- MSY.stats$SSB / MSY.stats$SSBmsy
  xlab <- expression(TRO/TRO[MSY])

  Uquantiles <- MSY.stats$FFmsyquantiles
  U <- MSY.stats$FFmsy
  ylab <- expression(paste(F/F[MSY], " (ages 2-15)"))

  F25 <- Uquantiles["25%",]
  Fmed <- Uquantiles["50%",]
  F75 <- Uquantiles["75%",]
  B25 <- Bquantiles["25%",]
  Bmed <- Bquantiles["50%",]
  B75 <- Bquantiles["75%",]

  nyears <- length(Bmed)
  years <- 1952:(1952 + nyears)

  df <- data.frame(Year = as.numeric(names(Bmed)),
                   B25 = B25, Bmed = Bmed, B75 = B75,
                   F25 = F25, Fmed = Fmed, F75 = F75) %>%
    dplyr::mutate(alpha = as.numeric(Year / max(Year)))

  df_thin <- dplyr::select(df, Year, Bmed, Fmed) %>%
    dplyr::mutate(Year = ifelse(Year %in% c(min(Year), max(Year), seq(0, 1e6, 10)), Year, ""))
  dft1 <- dplyr::select(df, Year, B25, F25) %>% dplyr::mutate(Year = "")
  names(dft1) <- names(df_thin)
  dft2 <- dplyr::select(df, Year, B75, F75) %>% dplyr::mutate(Year = "")
  names(dft2) <- names(df_thin)
  df_thin <- rbind(df_thin, dft1, dft2)

  # write.csv(df, "Kobe.csv")

  ggplot(df, aes(Bmed, Fmed)) +
    # geom_rect(xmin = -Inf, xmax = 1, ymin = -Inf, ymax = 1, fill = "yellow", alpha = 0.005) +
    # geom_rect(xmin = 1, xmax = Inf, ymin = -Inf, ymax = 1, fill = "green", alpha = 0.005) +
    # geom_rect(xmin = -Inf, xmax = 1, ymin = 1, ymax = Inf, fill = "red", alpha = 0.005) +
    # geom_rect(xmin = 1, xmax = Inf, ymin = 1, ymax = Inf, fill = "yellow", alpha = 0.005) +
    geom_errorbar(aes(ymin = F25, ymax = F75), alpha = 0.2) +
    geom_errorbarh(aes(xmin = B25, xmax = B75), alpha = 0.2) +
    geom_segment(aes(xend = c(tail(Bmed, n=-1), NA), yend = c(tail(Fmed, n=-1), NA), alpha = alpha), arrow = arrow(length = unit(0.1, "cm"))) +
    #ggrepel::geom_label_repel(data = df_thin, aes(Bmed, Fmed, label = Year), box.padding = unit(1.0, "lines"), min.segment.length = unit(2.0, "lines"), colour = "grey") +
    #ggrepel::geom_label_repel(data = df_thin, aes(Bmed, Fmed, label = Year), box.padding = unit(0.65, "lines"), min.segment.length = unit(0.0, "lines"), colour = "grey", alpha = 0.5, force = 35, max.iter = 4000) +
    ggrepel::geom_label_repel(data = df_thin, aes(Bmed, Fmed, label = Year), box.padding = unit(0.65, "lines"), colour = "grey", alpha = 0.7, min.segment.length = unit(0.0, "lines"), force = 3) +
    theme_bw(base_size = 16) +
    theme(legend.position = "none") +
    expand_limits(x = 0, y = 0) +
    scale_x_continuous(expand = c(0, 0), breaks = seq(0, 100, 1), minor_breaks = seq(0, 100, 0.5)) +
    scale_y_continuous(expand = c(0, 0)) +
    #coord_fixed() +
    labs(x = xlab, y = ylab)
}



#' Plots U/Umsy vs B/Bmsy over time with confidence intervals.
#'
#' Umsy and Bmsy were calculated by msycalc.tpl using anual weights, selectivities and allocation
#' Ana Parma, Bali 2011
#'
#' @author Ana Parma, Darcy Webber
#' @export
#'
plot_Kobe <- function(MSY.stats, use.B10 = TRUE, catch.over.B = FALSE) {
    if (use.B10 == FALSE) {
        Bquantiles <- MSY.stats$B.Bmsy.quantiles
        BBmsy <- MSY.stats$SSB / MSY.stats$SSBmsy
        xlab <- expression(SSB/SSB[MSY])
    } else { # use B10plus in plots
        Bquantiles <- MSY.stats$B10.B10msy.quantiles
        BBmsy <- MSY.stats$B10plus / MSY.stats$B10msy
        xlab <- expression(paste(B/B[MSY], " (ages 10+)"))
    }

    if (catch.over.B == TRUE) {
        Uquantiles = MSY.stats$UUmsyquantiles
        U <- MSY.stats$UUmsy
        ylab <- expression(U/U[MSY])
    } else {
        Uquantiles = MSY.stats$FFmsyquantiles
        U = MSY.stats$FFmsy
        ylab <- expression(paste(F/F[MSY], " (ages 2-15)"))
    }

    F25 <- Uquantiles["25%",]
    Fmed <- Uquantiles["50%",]
    F75 <- Uquantiles["75%",]
    B25 <- Bquantiles["25%",]
    Bmed <- Bquantiles["50%",]
    B75 <- Bquantiles["75%",]

    nyears <- length(Bmed)
    years <- 1952:(1952 + nyears)

    df <- data.frame(Year = as.numeric(names(Bmed)),
                     B25 = B25, Bmed = Bmed, B75 = B75,
                     F25 = F25, Fmed = Fmed, F75 = F75) %>%
        dplyr::mutate(alpha = as.numeric(Year / max(Year)))

    df_thin <- dplyr::select(df, Year, Bmed, Fmed) %>%
        dplyr::mutate(Year = ifelse(Year %in% c(min(Year), max(Year), seq(0, 1e6, 10)), Year, ""))
    dft1 <- dplyr::select(df, Year, B25, F25) %>% dplyr::mutate(Year = "")
    names(dft1) <- names(df_thin)
    dft2 <- dplyr::select(df, Year, B75, F75) %>% dplyr::mutate(Year = "")
    names(dft2) <- names(df_thin)
    df_thin <- rbind(df_thin, dft1, dft2)

    #write.csv(df[,1:7], "figs/Kobe.csv")

    ggplot(df, aes(Bmed, Fmed)) +
        # geom_rect(xmin = -Inf, xmax = 1, ymin = -Inf, ymax = 1, fill = "yellow", alpha = 0.005) +
        # geom_rect(xmin = 1, xmax = Inf, ymin = -Inf, ymax = 1, fill = "green", alpha = 0.005) +
        # geom_rect(xmin = -Inf, xmax = 1, ymin = 1, ymax = Inf, fill = "red", alpha = 0.005) +
        # geom_rect(xmin = 1, xmax = Inf, ymin = 1, ymax = Inf, fill = "yellow", alpha = 0.005) +
        geom_errorbar(aes(ymin = F25, ymax = F75), alpha = 0.2) +
        geom_errorbarh(aes(xmin = B25, xmax = B75), alpha = 0.2) +
        geom_segment(aes(xend = c(tail(Bmed, n=-1), NA), yend = c(tail(Fmed, n=-1), NA), alpha = alpha), arrow = arrow(length = unit(0.1, "cm"))) +
        #ggrepel::geom_label_repel(data = df_thin, aes(Bmed, Fmed, label = Year), box.padding = unit(1.0, "lines"), min.segment.length = unit(2.0, "lines"), colour = "grey") +
        #ggrepel::geom_label_repel(data = df_thin, aes(Bmed, Fmed, label = Year), box.padding = unit(0.65, "lines"), min.segment.length = unit(0.0, "lines"), colour = "grey", alpha = 0.5, force = 35, max.iter = 4000) +
        ggrepel::geom_label_repel(data = df_thin, aes(Bmed, Fmed, label = Year), box.padding = unit(0.65, "lines"), colour = "grey", alpha = 0.7, min.segment.length = unit(0.0, "lines"), force = 3) +
        theme_bw(base_size = 16) +
        theme(legend.position = "none") +
        expand_limits(x = 0, y = 0) +
        scale_x_continuous(expand = c(0, 0), breaks = seq(0, 100, 1), minor_breaks = seq(0, 100, 0.5)) +
        scale_y_continuous(expand = c(0, 0)) +
        #coord_fixed() +
        labs(x = xlab, y = ylab)
}
