#' Plot of model fit to the observed Australian age-frequencies by year.
#' 
#' Fit of the model sbtmodxx.tpl to the data for southern bluefin tuna. Fits of the observed Australian age data to the predicted Australian age data.
#'
#' @param labrep.file name of the input file
#' @param case_label an additional label for the plot
#' @author Darcy Webber
#' @export
#'
plot_Australian_age_fits <- function(labrep.file = "sbtmod_lab.rep", case_label = "")
{
   x <- readList(labrep.file)
   ages <- x$ages.2[1]:x$ages.2[2]
   years <- x$age.obs.2[,2]
   
   obs <- x$age.obs.2[,-c(1,2)]
   rownames(obs) <- years
   colnames(obs) <- ages
   obs <- reshape2::melt(obs) %>%
       dplyr::mutate(grp = 'Observed')
   
   pred <- x$age.pred_2[,-c(1,2)]
   rownames(pred) <- years
   colnames(pred) <- ages
   pred <- reshape2::melt(pred) %>%
       dplyr::mutate(grp = 'Expected')

   d <- rbind(obs, pred)
   head(d); tail(d)
   
   ggplot2::ggplot() +
       ggplot2::geom_point(data = d[d$grp %in% 'Observed',], aes(x = Var2, y = value, group = grp, colour = grp, shape = grp)) +
       ggplot2::geom_line(data = d[d$grp %in% 'Expected',], aes(x = Var2, y = value, group = grp, colour = grp, linetype = grp)) +
       ggplot2::facet_wrap(~Var1, dir = 'v') +
       ggplot2::labs(x = 'Age', y = 'Proportion', title = "Australian age composition data", subtitle = case_label) +
       ggplot2::scale_colour_manual("", values = c('Observed' = 'red', 'Expected' = 'black')) +
       #ggplot2::scale_linetype_manual("", values = c('Observed' = NA, 'Expected' = 1)) +
       #ggplot2::scale_shape_manual("", values = c('Observed' = 16, 'Expected' = NA)) +
       ggplot2::theme_bw() +
       ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", hjust = 0.5), plot.subtitle = ggplot2::element_text(hjust = 0.5)) +
       ggplot2::theme(legend.position = "none")
}


#' Plot of model fit to the observed Australian age-frequencies by year.
#' 
#' Fit of the model sbtmodxx.tpl to the data for southern bluefin tuna. Fits of the observed Australian age data to the predicted Australian age data.
#'
#' @param labrep.file name of the input file
#' @param case_label an additional label for the plot
#' @return a plot
#' @examples
#' pdf("figs/AussieAgeFts.pdf", width = 6, height = 11.5)
#' plot_aussie_age_fits(labrep.file = "sbtmod_lab.rep", case_label = "c1")
#' dev.off()
#' @export
#'
plot_Australian_age_fits_old <- function(labrep.file = "sbtmod_lab.rep", case_label = "")
{
   subtle.color <- "gray40"
   x <- readList(labrep.file)
   ages <- x$ages.2
   obs.data <- x$age.obs.2[,-c(1,2)]
   pred.data <- x$age.pred_2[,-c(1,2)]
   years <- x$age.obs.2[,2]
   
   nyears <- length(years)
   ages.list <- ages[1]:ages[2]
   nages <- length(ages.list)

   mfcol <- c(ceiling(nyears/3),3)
   par(mfcol=mfcol,oma=c(3.5,4.5,3.5,1),mar=c(0,0,0,0))
   cohort.color <- rainbow(mfcol[1]+2)[-c(1:2)]   #use hideous rainbow colors because they loop more gracefully than rich.colors
   ncolors <- length(cohort.color)
   
   ylim <- c(0,1.05*max(obs.data,pred.data))
   for (yr in 1:nyears)
   {
      names.arg <- rep("",nages)
      x <- barplot(obs.data[yr,],space=0.2,ylim=ylim,las=1,names.arg=names.arg, cex.names=0.5, xaxs="i",yaxs="i",border=subtle.color, col=cohort.color[1:nages],axes=F,ylab="",xlab="")
      cohort.color <- c(cohort.color[ncolors],cohort.color[-1*ncolors])  #loop around colors
      if (yr %% mfcol[1] == 0)
      {
         axis(side=1,at=x,lab=ages.list, line=-0.1,col.axis=subtle.color, col=subtle.color,lwd=0,lwd.ticks=0)  #just use for the labels, to allow more control than names.arg
      }
      if (yr <= mfcol[1])
      {
        axis(2,las=1,at=c(0,0.5),col=subtle.color,col.axis=subtle.color,lwd=0.5)
      }
      par(new=T)
      par(xpd=NA)
      plot(x=x,y=pred.data[yr,],ylim=ylim, xlim=par("usr")[1:2], las=1,xaxs="i",yaxs="i",col="black",bg="white",pch=19,cex=1.3,axes=F,ylab="",xlab="")
      box(col=subtle.color,lwd=0.5)
      x.pos <- par("usr")[1] + 0.85*diff(par("usr")[1:2])   #par("usr") spits out the current coordinates of the plot window
      y.pos <- par("usr")[3] + 0.75*diff(par("usr")[3:4])   #par("usr") spits out the current coordinates of the plot window
      text(x=x.pos,y=y.pos,years[yr],cex=1.2, col=subtle.color)
      par(xpd=T)
   }
   mtext(side=1,outer=T,"Age",line=2)
   mtext(side=2,outer=T,"Proportion",line=3.2)
   mtext(side=3,outer=T,line=1.2,"Australian age composition data")
   mtext(side=3,outer=T,line=0.2,paste("(",case_label,")",sep=""),cex=0.6)
}


#' Plot of model fit to the observed Indonesian age-frequencies by year.
#' 
#' Fit of the model sbtmodxx.tpl to the data for southern bluefin tuna.
#'
#' @return a plot
#' @export
#'
plot_Indonesian_age_fits <- function(data.object, case_label = "c1")
{
   library(PBSmodelling)
   library(gplots)
   subtle.color <- "gray40"
   x <- data.object
   ages <- x$ages.1 # age range
   obs.data <- x$age.obs.1[,-c(1,2)]
   pred.data <- x$age.pred_1[,-c(1,2)]
   years <- x$age.obs.1[,2]
   
   nyears <- length(years)
   ages.list <- ages[1]:ages[2]
   nages <- length(ages.list)
   mfrow <- c(nyears,1)
   par(mfrow=mfrow,oma=c(4,5.5,3.5,1),mar=c(0,0,0,0))
   
   cohort.color <- rich.colors(nages+nyears)   
   ylim <- c(0,1.05*max(obs.data,pred.data))
   for (yr in 1:nyears)
   {
      names.arg <- rep("",nages)      
      x <- barplot(obs.data[yr,],space=0.2,ylim=ylim,las=1,names.arg=names.arg, cex.names=0.5, xaxs="i",yaxs="i",col=cohort.color[(nyears+1):(nages+nyears)],
                        axes=F,ylab="",xlab="", border=subtle.color,lwd=0.5)
      cohort.color <- c(cohort.color[length(cohort.color)],cohort.color[-1*length(cohort.color)])  #loop around colors
      if (yr >= nyears)
      {
         axis(side=1,at=x,lab=ages.list, col.axis=subtle.color, col=subtle.color, lwd=0,lwd.ticks=0)  #just use for the labels, to allow more control than names.arg
      }
      axis(2,las=1,at=c(0,0.1), col=subtle.color, col.axis=subtle.color,lwd=0.5)
      par(new=T)
      par(xpd=NA)
      plot(x=x,y=pred.data[yr,],ylim=ylim, xlim=par("usr")[1:2], las=1,xaxs="i",yaxs="i",col="black",pch=19,cex=1.3,axes=F,ylab="",xlab="")
      box(col=subtle.color,lwd=0.5)
      x.pos <- par("usr")[1] + 0.95*diff(par("usr")[1:2])   #par("usr") spits out the current coordinates of the plot window
      y.pos <- par("usr")[3] + 0.8*diff(par("usr")[3:4])   #par("usr") spits out the current coordinates of the plot window
      text(x=x.pos,y=y.pos,years[yr],cex=1.1, col=subtle.color)
      par(xpd=T)
   }
   mtext(side=1,outer=T,"Age",line=2.5)
   mtext(side=2,outer=T,"Proportion",line=3.5)
   mtext(side=3,outer=T,line=1.2,"Indonesian age composition data")
   mtext(side=3,outer=T,line=0.2,paste("(",case_label,")",sep=""),cex=0.6)
}
