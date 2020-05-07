#' Plot recruitment and SSB
#'
#' @param data.objects a list of the _lab.rep files from all grid cells
#' @author Ana Parma, Darcy Webber
#' @import patchwork
#' @export
#'
plot_recruitment_and_ssb <- function(data.objects) {
   nobjects <- length(data.objects)
   recdevs <- NULL

   steep <- NULL
   M1 <- NULL
   M10 <- NULL
   SSB <- NULL
   R <- NULL
   alpha <-NULL
   beta <- NULL
   sigmaR <- NULL
   xx <- data.objects[[1]]
   n <- length(xx$Recruitment)

   for (i in 1:nobjects) {
      xx <- data.objects[[i]]
      recdevs <- rbind(recdevs, xx$Rdev)
      steep <- rbind(steep, xx$steep)
      M1 <- rbind(M1, xx$M[2])
      M10 <- rbind(M10, xx$M[11])
      R <- rbind(R, xx$Recruitment[-n])
      SSB <- rbind(SSB, xx$Sbio[-n]/xx$Sbio[1])
      alpha <-rbind(alpha, xx$alpha[2])
      beta <- rbind(beta, xx$beta[2]/xx$Sbio[1])
      sigmaR <- rbind(sigmaR, xx$sigma.r)
   }

   years <- xx$years[1]:xx$years[2]

   steep <- data.frame(steep)
   steep$grp <- 1:nrow(steep)
   alpha <- data.frame(alpha)
   alpha$grp <- 1:nrow(alpha)
   beta <- data.frame(beta)
   beta$grp <- 1:nrow(beta)
   sigmaR <- data.frame(sigmaR)
   sigmaR$grp <- 1:nrow(sigmaR)
   M1 <- data.frame(M1)
   M1$grp <- 1:nrow(M1)
   M10 <- data.frame(M10)
   M10$grp <- 1:nrow(M10)

   Rec <- data.frame(R)
   names(Rec) <- years
   Rec$grp <- 1:nrow(Rec)
   Rec <- tidyr::gather(Rec, key = Year, value = Recruitment, -grp) %>%
       dplyr::mutate(Year = as.integer(Year)) %>%
       dplyr::full_join(M1) %>%
       dplyr::full_join(M10) %>%
       dplyr::full_join(alpha) %>%
       dplyr::full_join(beta) %>%
       dplyr::full_join(sigmaR) %>%
       dplyr::full_join(steep)

   TRO <- data.frame(SSB)
   names(TRO) <- years
   TRO$grp <- 1:nrow(TRO)
   TRO <- tidyr::gather(TRO, key = Year, value = TRO, -grp) %>%
       dplyr::mutate(Year = as.integer(Year))

   d <- dplyr::full_join(Rec, TRO) %>%
       dplyr::mutate(aa = alpha * exp(0.5 * sigmaR))

   p1 <- ggplot(d, aes(Year, Recruitment, group = grp)) +
       geom_path(aes(colour = factor(M1)), alpha = 0.2) +
       theme_bw() +
       labs(colour = "M1") +
       scale_x_continuous(breaks = seq(0, 1e6, 10), minor_breaks = seq(0, 1e6, 1))
   p2 <- ggplot(d, aes(Year, TRO, group = grp)) +
       geom_path(aes(colour = factor(M10)), alpha = 0.2) +
       theme_bw() +
       labs(colour = "M10") +
       scale_x_continuous(breaks = seq(0, 1e6, 10), minor_breaks = seq(0, 1e6, 1))
   p1 + p2
}


#' Plot SR
#'
#' @param data.objects a list of the _lab.rep files from all grid cells
#' @author Ana Parma, Darcy Webber
#' @export
#'
plot_SR <- function(data.objects)
{
   nobjects <- length(data.objects)
   recdevs <- NULL

   steep <- NULL
   M1 <- NULL
   M10 <- NULL
   SSB <- NULL
   R <- NULL
   alpha <-NULL
   beta <- NULL
   sigmaR <- NULL
   xx <- data.objects[[1]]
   n <- length(xx$Recruitment)

   for (i in 1:nobjects)
   {
      xx <- data.objects[[i]]
      recdevs <- rbind(recdevs, xx$Rdev)
      steep <- rbind(steep, xx$steep)
      M1 <- rbind(M1, xx$M[2])
      M10 <- rbind(M10, xx$M[11])
      R <- rbind(R, xx$Recruitment[-n])
      SSB <- rbind(SSB, xx$Sbio[-n]/xx$Sbio[1])
      alpha <-rbind(alpha, xx$alpha[2])
      beta <- rbind(beta, xx$beta[2]/xx$Sbio[1])
      sigmaR <- rbind(sigmaR, xx$sigma.r)
   }

   years <- xx$years[1]:xx$years[2]

   steep <- data.frame(steep)
   steep$grp <- 1:nrow(steep)
   alpha <- data.frame(alpha)
   alpha$grp <- 1:nrow(alpha)
   beta <- data.frame(beta)
   beta$grp <- 1:nrow(beta)
   sigmaR <- data.frame(sigmaR)
   sigmaR$grp <- 1:nrow(sigmaR)
   M1 <- data.frame(M1)
   M1$grp <- 1:nrow(M1)
   M10 <- data.frame(M10)
   M10$grp <- 1:nrow(M10)

   Rec <- data.frame(R)
   names(Rec) <- years
   Rec$grp <- 1:nrow(Rec)
   Rec <- tidyr::gather(Rec, key = Year, value = Recruitment, -grp) %>%
       dplyr::mutate(Year = as.integer(Year)) %>%
       dplyr::full_join(M1) %>%
       dplyr::full_join(M10) %>%
       dplyr::full_join(alpha) %>%
       dplyr::full_join(beta) %>%
       dplyr::full_join(sigmaR) %>%
       dplyr::full_join(steep)

   TRO <- data.frame(SSB)
   names(TRO) <- years
   TRO$grp <- 1:nrow(TRO)
   TRO <- tidyr::gather(TRO, key = Year, value = TRO, -grp) %>%
       dplyr::mutate(Year = as.integer(Year))

   d <- dplyr::full_join(Rec, TRO) %>%
       dplyr::mutate(aa = alpha * exp(0.5 * sigmaR))
   d2 <- expand.grid(grp = unique(d$grp), Year = unique(d$Year), ss = seq(0, 1, 0.01)) %>%
       dplyr::full_join(d) %>%
       dplyr::mutate(rr = aa * ss/(beta + ss))

   ggplot(d, aes(TRO, Recruitment, group = grp)) +
       geom_path(alpha = 0.3) +
       geom_path(data = d2, aes(ss, rr, colour = factor(steep)), alpha = 0.3) +
       facet_grid(M1 ~ M10) +
       theme_bw() +
       labs(x = "SSB", colour = "h")
}
