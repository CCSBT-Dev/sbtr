#' Length-frequency residuals
#'
#' @export
#' 
plot_length_resid_LL <- function(labrep.file = "sbtmod_lab.rep", case_label = "c1", LL = 1)
{
   x <- readList(labrep.file)
   lens <- seq(from = x$lengths[1], by = x$lengths[2], length.out = x$lengths[3])
   years <- x$len.pred[x$len.pred[,1] == LL, 2]   
   obs <- x$len.obs[x$len.obs[,1] == LL, -c(1,2)]  #extract the lengths where the fishery is =1, and exclude the fishery and year
   pred <- x$len.pred[x$len.pred[,1] == LL, -c(1,2)]
   res <- x$len.res[x$len.res[,1] == LL, -c(1,2)]
   #res <- obs - pred# / sqrt(pred * (1.0 - pred / pred_lf_effN_is[i,s])
   #res <- obs - pred# / sqrt(var(pred))

   rownames(res) <- years
   colnames(res) <- lens
   res <- reshape2::melt(res, value.name = "Residual") %>%
       dplyr::mutate(Sign = ifelse(Residual < 0, '-', '+')) %>%
       dplyr::mutate(Residual = abs(Residual))
   
   ggplot2::ggplot(data = res, aes(x = Var1, y = Var2, size = Residual, color = Sign)) +
       ggplot2::geom_point() +
       labs(x = 'Year', y = 'Length (cm)') +
       ggplot2::theme_bw()
}


#' Predicted length frequency for longline fishery 1 to the observed data.
#' 
#' Fit of the model sbtmodxx.tpl to the data for southern bluefin tuna.
#'
#' @examples
#' pdf("figs/LengthsLL1.pdf", width = 8, height = 11.5)
#' LengthsLL1(labrep.file = "sbtmod_lab.rep", case_label = "c1")
#' dev.off()
#' @export
#' 
plot_length_fits_LL <- function(labrep.file = "sbtmod_lab.rep", case_label = "c1", LL = 1)
{
   subtle.color <- "gray40"
   x <- readList(labrep.file)
   length.list <- seq(from=x$lengths[1],by=x$lengths[2],length.out=x$lengths[3])
   obs.data <- x$len.obs[x$len.obs[,1]==LL,-c(1,2)]  #extract the lengths where the fishery is =1, and exclude the fishery and year
   pred.data <- x$len.pred[x$len.pred[,1]==LL,-c(1,2)]
   years <- x$len.pred[x$len.pred[,1]==LL,2]
   nyears <- length(years)
   nlengths <- length(length.list)
   mfcol <- c(ceiling(nyears/5),5)
   par(mfcol=mfcol,oma=c(3.8,4.5,3.5,1),mar=c(0,0,0,0))
   ylim <- c(0,1.05*max(obs.data,pred.data))
   cohort.color <- rich.colors(nlengths)
   #these used for drawing outside line on histogram
   xvals <- vector(length=2*nlengths)
   yvals <- vector(length=2*nlengths)
   for (yr in 1:nyears)
   { 
      names.arg <- rep("",nlengths)
      #histogram lines and bars the same color and no gaps between them
      x <- barplot(obs.data[yr,],space=0,ylim=ylim,las=1,names.arg=names.arg, cex.names=0.5, xaxs="i",yaxs="i",border=cohort.color,
                  col=cohort.color,axes=F,ylab="",xlab="")      
      #do some fancy footwork to get top line of histogram drawn but not lines in between bins
      bin.width <- x[2]-x[1]
      bw2 <- bin.width/2
      for (i in 1:nlengths)
      {
         xvals[2*i-1] <- x[i]-bw2            
         xvals[2*i] <- x[i]+bw2
         yvals[2*i-1] <- obs.data[yr,i]
         yvals[2*i] <- obs.data[yr,i]
      }
      lines(xvals,yvals,col=subtle.color,lwd=0.5)      
      #now plot the axes
      if (yr %% mfcol[1] == 0 || yr==nyears)
      {
         axis(side=1,at=x,lab=length.list, line=-0.1,col.axis=subtle.color, col=subtle.color,lwd=0,lwd.ticks=0)  #just use for the labels, to allow more control than names.arg
      }
      if (yr <= mfcol[1])
      {
        axis(2,las=1,at=c(0,0.15),col=subtle.color,col.axis=subtle.color,lwd=0.5)
      }
      par(new=T)
      par(xpd=NA)
      plot(x=x,y=pred.data[yr,],ylim=ylim, xlim=par("usr")[1:2], las=1,xaxs="i",yaxs="i",col="black",bg="white",pch=19,cex=0.6,axes=F,ylab="",xlab="")
      box(col=subtle.color,lwd=0.5)
      x.pos <- par("usr")[1] + 0.15*diff(par("usr")[1:2])   #par("usr") spits out the current coordinates of the plot window
      y.pos <- par("usr")[3] + 0.82*diff(par("usr")[3:4])   #par("usr") spits out the current coordinates of the plot window
      text(x=x.pos,y=y.pos,years[yr],cex=1.2, col=subtle.color)
      par(xpd=T)
   }
   mtext(side=1,outer=T,"Lengths",line=2.3)
   mtext(side=2,outer=T,"Proportions",line=3.2)
   mtext(side=3,outer=T,line=1.2,"Longline 1 length frequency")
   mtext(side=3,outer=T,line=0.2,paste("(",case_label,")",sep=""),cex=0.6)
}
