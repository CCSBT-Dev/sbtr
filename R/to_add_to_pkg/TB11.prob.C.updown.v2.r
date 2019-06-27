#' Prob C updown version 2
#' 
#' @param s3file vector of output filename ending in .s3 from one of the projections, c("xxx.s4","yyy.s4")
#' @param xlim years to select
#' @export
#' 
prob.C.updown <- function(s3file, xlim = c(2012,2022))
{
   tmp <- s3file
   prob.switches = vector(length=2)
   data.cols <- c(grep(paste("C.",xlim[1],sep=""),colnames(tmp)):grep(paste("C.",xlim[2],sep=""),colnames(tmp)))
   worms <- as.matrix(tmp[,data.cols])/1e+03
   id.catch.breaks <- c(2012,2013,2016,2019,2022)+1-xlim[1]
   worms <- worms[,id.catch.breaks]
   ncols <- ncol(worms)
   nreplicates <- nrow(worms)
   differences <- worms[,2:(ncols)]-worms[,1:(ncols-1)]
   first2 <- differences[,1]>0.14 & differences[,2]<0
   first4 <- (differences[,1]>0.14 & differences[,2]<0) | (differences[,2]>0 & differences[,3]<0)|(differences[,3]>0 & differences[,4]<0)
   prob.switches[1]=round(length(first2[first2==TRUE])/nreplicates,4)
   prob.switches[2]=round(length(first4[first4==TRUE])/nreplicates,4)
 return(prob.switches)
}

