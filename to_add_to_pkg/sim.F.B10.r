#' Create simulated series from resampled lev file
#'
#' Requires get.cov.est(), MASS (for multivariate normal)
#' 
#' @param directory the directory that the ADMB .par/.std/.cor files are in
#' @return a list of 2 objects, F and B10 sampled over grids 
#' @author Jim Ianelli, Darcy Webber,  Ana Parma
#' @examples
#' # where myM is a list containting lev index and cov and est
#' simF.B10 <- sim.F.B10(covrep,lev.file = "arc/grids/base2013sqrt.lev")
#' @export
#' 
sim.F.B10 <- function(myM,lev.file = "arc/grids/base2013sqrt.lev",ngrids=320)
{
  require(MASS)
  # lev.file="../OM/arc/grids/base2013sqrt.lev"
  x <- read.csv(file=lev.file,header=F,colClasses="numeric",sep=" ")
  ntmp=nchar(lev.file[1])-4
  lev.file <- strtrim(lev.file,ntmp)
  nlevs <- nrow(x)
  lev.scens = 1:nlevs
  for (i in 1:nlevs) 
    lev.scens[i] <- as.numeric(paste(x[i,1],x[i,2],x[i,3],x[i,4],x[i,5],x[i,6],sep=""))
  x <- table(lev.scens)
  # Set up dimensions for F and B10 (they are different lengths)
  nests <- length(myM[[1]]$est)
  nB    <- length(myM[[1]]$B_idx)
  nF    <- length(myM[[1]]$F_idx)
  nR    <- length(myM[[1]]$R_idx)
  sim_B <- data.frame(matrix(0,nlevs,nF))
  sim_F <- data.frame(matrix(0,nlevs,nF))
  sim_R <- data.frame(matrix(0,nlevs,nR))
  irow  <- 1
  for (i in 1:ngrids)
  {
    nreps <- x[ myM[[i]]$lev ]
    if (!is.na(nreps))
    {
      # Do sims
      Mtmp <- matrix(mvrnorm(nreps,myM[[i]]$est,myM[[i]]$cov),nreps,nests)
      print(dim(Mtmp))
      # Ftmp <- Mtmp[,myM[[i]]$F_idx]
      # Btmp <- Mtmp[,myM[[i]]$B_idx]
      # print(paste(dim(Btmp),dim(Ftmp)))
      # AllSims[irow:(irow+nreps-1),] <- cbind(Btmp,Ftmp)
      # sim_B[irow:(irow+nreps-1),] <- Mtmp[,1:nB]
      sim_B[irow:(irow+nreps-1),] <- Mtmp[,1:(nB-1)]
      sim_F[irow:(irow+nreps-1),] <- Mtmp[,(nB):(nB+nF-1)]
      sim_R[irow:(irow+nreps-1),] <- Mtmp[,(nB+nF):(nB+nF+nR-1)]
      irow <- irow + nreps 
      # print(paste(i,Mtmp[irow,1:4]))
      # print(l1)
    }
  }
  return(list("B10"=sim_B,"F"=sim_F,"R"=sim_R))
}

