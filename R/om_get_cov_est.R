#' Load covariance and estimates R
#' 
#' Goes to the specified directory and finds the .est and .cov files. 
#' Loads them one by one into an R list of objects. This ensures that
#' the reading in stage (which can take 3-10 minutes) need only be done once, 
#' and then subsequent function calls looping through an entire grid will run 
#' in a few seconds instead of minutes. Required the following files: something 
#' ending in .cor and .std
#' 
#' @param directory the directory that the ADMB .par/.std/.cor files are in
#' @return a list of objects dimensioned by number of grids 
#' @author Jim Ianelli, Darcy Webber,  Ana Parma
#' @examples
#' grid.cov.est <- get_grid_cov_est(directory = "arc/basesqrt")
#' @export
#' 
get_cov_est <- function(directory = "../OM/arc/base2016_sqrt")
{
  # Need to test if already loaded...
  fn        <- dir(directory, pattern = ".par")
  ntmp      <- nchar(fn[1])-4
  fn        <- strtrim(fn,ntmp)
  ntmp      <- nchar(fn[1])-14
  levn      <- substr(fn,ntmp+1,200)
  ngrids    <- length(fn)
  # set up the list
  x <- list("F_idx"=0,"B_idx"=0,"lev"=0,"est"=0,"cov"=0,"Nest"=0,"Ncov"=0)
  M <- rep(list(x),ngrids)
#  for (i in 1:3)
  for (i in 1:ngrids)
  {
    # this reads in the correlation matrix etc
    mf         <- read.fit(paste(directory,"/",fn[i],sep=""))
    # set up indices for extraction
    noquote(unlist(strsplit(fn,"[hmMOCap]")))
    tmp <- as.vector((unlist(strsplit(levn[i],"[hmMOCap]"))))
    M[[i]]$lev <- paste(tmp[2],tmp[3],tmp[4],tmp[5],tmp[6],tmp[7],tmp[8],sep="")
    idx_B10    <- mf$names=="B10_plus"
    idx_F      <- mf$names=="F_a215"
    idx_R      <- mf$names=="Recs"
    nB10       <- sum(idx_B10)
    nF         <- sum(idx_F)
    nR         <- sum(idx_R)
    idx_all    <- idx_B10 | idx_F | idx_R
    idx_nage   <- mf$names=="sdNage"
    
    # Index ranges for variables of interest
    Bs         <- c(1,nB10)
    Fs         <- c((nB10+1),(nB10+idx_F))
    Rs         <- c(Fs[2]+1,sum(idx_all))
    M[[i]]$B_idx <- Bs[1]:Bs[2]
    M[[i]]$F_idx <- Fs[1]:Fs[2]
    M[[i]]$R_idx <- Rs[1]:Rs[2]
    M[[i]]$est <- mf$est[idx_all]
    M[[i]]$cov <- mf$cov[idx_all,idx_all]
    M[[i]]$Nest<- mf$est[idx_nage]
    M[[i]]$Ncov<- mf$cov[idx_nage,idx_nage]
    print(paste("Finished",i,"out of",ngrids))
  }
  return(M)
}
