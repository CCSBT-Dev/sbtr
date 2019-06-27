#' Computes and plots SPR
#' 
#' @author Ana Parma
#' @examples
#' Plot.surv.R(base25)
#' @export
#' 
Plot.surv.R <- function(data.objects) 
{
  # data.objects is a list of the _lab.rep files from all grid cells
  # produced with get.all.data.r

   xx <- data.objects[[1]]
   years=xx$yrs.catch[1]:xx$yrs.catch[2]
   nyears = length(years)
   nobjects <- length(data.objects)
   allyears = xx$years[1]:xx$years[2]
  
   Surv10 = matrix(NA,nobjects,nyears)  
   Surv15 = matrix(NA,nobjects,nyears)  
   Surv20 = matrix(NA,nobjects,nyears)  

    scenario = vector(length=nobjects)
   for (i in 1:nobjects)
   {
      xx <- data.objects[[i]]
      fishery = xx$Fs[,1]
      scenario[i] = xx$scenario_number
   # harvest rate by season
      H1 = xx$Fs[fishery ==3,3:33]+xx$Fs[fishery==4,3:33]+xx$Fs[fishery==5,3:33]+xx$Fs[fishery==6,3:33]
      H2 = xx$Fs[fishery ==1,3:33]+xx$Fs[fishery==2,3:33]
      M = xx$M
      S = matrix(exp(-M),nyears,31,byrow=T)
      Surv = (1-H1)*(1-H2)*S
      Surv10[i,] <- Surv[,11] 
      Surv15[i,] <- Surv[,16] 
      Surv20[i,] <- Surv[,21] 
    }
    boxplot(Surv10,names=years,cex=0.4,col=3,main="Surv10")
    abline(h=0.80)
    abline(h=0.75)

    windows()
    boxplot(Surv15,names=years,cex=0.4,col=3,main="Surv15")
    abline(h=0.80)
    abline(h=0.75)

    windows()
   boxplot(Surv20,names=years,cex=0.4,col=3,main="Surv20")
    abline(h=0.80)
   abline(h=0.75)

  return(list("Surv10"=Surv10,"Surv15"=Surv15,"Surv20"=Surv20))
}
