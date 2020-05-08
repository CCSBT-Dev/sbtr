#' Prob C updown version 1
#' 
#' @param s3files vector of output filename ending in .s3 from one of the projections, c("xxx.s4","yyy.s4")
#' @param ylim a vector giving the lower and upper bound to use for the y-axis 
#' @param xlim years to select
#' @param cpue.series 0=c0, 1=c1, 2=c2, 3=c3 corresponding to different assumptions about how the cpue is affected by misreporting. 
#' @param header title to put on top of plot
#' @param col.median color of median line
#' @param filename if 1, uses the file in the tree, if a file is specified, uses that, e.g. filename="Cfull2_H60.s4"
#' @export
#' 
prob.C.updown.v1 <- function(s3files,xlim=c(2012,2022))
{
 .Options$warn <- -1  # suppress annoying warning messages
 min.yr <- 1931   #could loop backward through yr looking for length(grep(yr,colnames(tmp))==0, to automate this
 
 nfiles <- length(s3files)
 prob.switches = matrix(0,nfiles,2)
  for (i in 1:nfiles) 
  {
   tmp <- read.table(s3files[i],skip=2,header=T)
   data.cols <- c(grep(paste("C.",xlim[1],sep=""),colnames(tmp)):grep(paste("C.",xlim[2],sep=""),colnames(tmp)))
   worms <- as.matrix(tmp[,data.cols])/1e+03
   id.catch.breaks <- c(2012,2013,2016,2019,2022)+1-xlim[1]
   worms <- worms[,id.catch.breaks]
   ncols = ncol(worms)
   nreplicates = nrow(worms)
   differences <- worms[,2:(ncols)]-worms[,1:(ncols-1)]
   first2 <- differences[,1]>0 & differences[,2]<0
   first4 <- (differences[,1]>0 & differences[,2]<0) | (differences[,2]>0 & differences[,3]<0)|(differences[,3]>0 & differences[,4]<0)
   prob.switches[i,1]=round(length(first2[first2==TRUE])/nreplicates,2)
   prob.switches[i,2]=round(length(first4[first4==TRUE])/nreplicates,2)
  } 
 return(prob.switches)
}
