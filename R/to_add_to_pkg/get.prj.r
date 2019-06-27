#' Read .prj files into R
#' 
#' Reads *.prj file and samples results using *.lev file to get distributions of several stats over the sampled grid if sample=F just reads *.prj file without sampling
#' 
#' @author Ana Parma
#' @examples
#' base2013.prj <- get.prj.R(file = "base2013")
#' @export
#' 
get.prj.R <- function(file = "base2013", first.yr = 1931, last.yr = 2012, sample = TRUE)
{
   lev.file <- paste(file,".lev",sep="")
   prj.file <- paste(file,".prj",sep="")
   pp       <- read.table(prj.file)
   
   var.names = c("scenario.number","F",
   "steep.level","m0.level","m10.level","omega.level","which.cpue","a.level",
   "B0","R0","alpha","beta","steep","emp.sigma.r","R.ac","R.resid",
   "lnq","emp.sigma.cpue","cpue.ac1","cpue.resid","adjust.cpue",
   "lnq.aerial","emp.sigma.aerial","aerial.ac","aerial.resid","sel.aerial",
   "Nage.last.yrplus1","Mage","sel.LL1",
   "sel.LL2","sel.Indo","sel.surf",
   "C.last.yr","catch.split",
   "cpue.psi","cpue.omega","cpue.a1","cpue.a2","sr.sp","aussieP",
   "TBmsy","MSY","SSBmsy","Fmsy_a215","F_a215","SSB","R")
   n =last.yr+1-first.yr+1 
   dimensions = c(1,1,
         1,1,1,1,1,1,
         1,1,1,1,1,1,1,1,
         1,1,1,1,1,
         1,1,1,1,3,
         31,31,31,
         31,31,31,
         1,4,
         1,1,1,1,1,1,
         1,1,1,1,1,n,n) 
  d1 = c(1,1+cumsum(dimensions[-length(dimensions)]))
  d2=d1+dimensions-1
  output=vector("list",length(var.names))
  names(output)=var.names
  

  for (i in 1:length(var.names))
  {
    output[[i]] = pp[,d1[i]:d2[i]]
  }  
  colnames(output$R)=c(first.yr:(last.yr+1))
  colnames(output$SSB)=c(first.yr:(last.yr+1))
  if(sample)
  {
    x <- read.csv(file=lev.file,header=F,colClasses="numeric",sep=" ")
    nlevs <- nrow(x)
    lev.scens <- vector(length=nlevs)
    for (i in 1:nlevs) 
    {
      lev.scens[i] <- as.numeric(paste(x[i,1],x[i,2],x[i,3],x[i,4],x[i,5],x[i,6],sep=""))
    }
    resamps <- match(lev.scens,output$scenario.number)
   
    for (i in 1:length(var.names))
    {
      if(dimensions[i] == 1) output[[i]] = output[[i]][resamps]
      if(dimensions[i] > 1) output[[i]] = output[[i]][resamps,]
    }  
  }
  return(output)
}
