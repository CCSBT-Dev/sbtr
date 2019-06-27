#' Plot ASP
#'
#' @param data.objects is the collection of lab.rep files
#' @examples
#' ASP <- Plot.ASP.R(base25)
#' @export
#' 
Plot.ASP.R <- function(data.objects, lev.file="base.lev",a1=2,a2=15,plot.yr1=1955) 
{
   xx <- data.objects[[1]]
   years=xx$yrs.catch[1]:xx$yrs.catch[2] 
   nyears = length(years)
   nobjects <- length(data.objects)
   allyears = xx$years[1]:xx$years[2]
   nallyears = length(allyears)
   TOTcatch = rep(0,nallyears)
   TOTcatch[allyears >= xx$yrs.catch[1]] = apply(xx$catch.weight.pred,2,sum)
    
   CSProd = matrix(NA,nobjects,nallyears)  

    scenario = vector(length=nobjects)
   for (i in 1:nobjects)
   {
     xx <- data.objects[[i]]
     scenario[i] = xx$scenario_number
     TOTbio = xx$TOTbio
     CSProd[i,]=  TOTcatch + diff(TOTbio)      
   }       
    
    lev <- read.table(file=lev.file,colClasses="numeric",sep=" ")
    nlevs = nrow(lev)    # 
    lev.scens <- as.numeric(paste(lev[,1],lev[,2],lev[,3],lev[,4],lev[,5],lev[,6],sep=""))
 
   resamps <- match(lev.scens,scenario)
   result= CSProd[resamps,]
 # par(mfrow=c(3,1),mar=c(3,3,1,1))
  id.years <- allyears >= plot.yr1 
  boxplot(data.frame(result[,id.years]),names=allyears[id.years],cex=0.4,col=3)
  mtext("Surplus production by year",3,1)
  result = as.data.frame(result)
  names(result) = allyears
  return(result)
}
