#' Reads parameters from lab.rep files and writes for the year selected
#' 
#' Reads parameters from lab.rep files and writes for the year selected all myscalc.dat for MSY runs that failed to converge (estimated MSY <1). Files are stored in subdirectory "msy" with the name of the corresponding scenario.
#' 
#' @author Ana Parma
#' @examples
#' write.MSY.dat.file(base25, year = 2005)
#' @export
#' 
write.MSY.dat.file.R <- function(data.objects, year = 2005) 
{
   xx <- data.objects[[1]]
   years=xx$yrs.catch[1]:xx$yrs.catch[2] 
   nyears = length(years)
   id.year = year-xx$yrs.catch[1]+1
   id.allyears = year-xx$years[1]+1
   nobjects <- length(data.objects)
   dir.create("msy")
   unlink("msy/*")     #delete file sin directory

   weights <- matrix(0,6,31)
   
   for (i in 1:nobjects)
   {
      
      xx <- data.objects[[i]]
        scenario=xx$scenario_number
        filename = paste("msy//msy",scenario,".dat",sep="")

      if(xx$MSY[id.year]<1)
      {
        write(6,filename)
        write(30,filename,append=T)
        catch = xx$catch.weight.pred[,id.year]
        catch.split = catch/sum(catch)
        id = catch.split > 0.002
        rows = (c(1:6)-1)*nyears+id.year
        sel = xx$sel[rows,3:33]
        sel = sel/matrix(apply(sel,1,max),6,31,byrow=F)
        Fs = xx$Fs[rows,3:33]
        weights = matrix(0,6,31)
        weights[1,]=xx$weights.ageLL1[id.allyears,]
        weights[2,]=xx$weights.ageLL2[id.allyears,]
        weights[3,]=xx$spweights.age[id.allyears,]
        weights[4,]=xx$spweights.age[id.allyears,]
        weights[5,]=xx$spweights.age[id.allyears,]
        weights[6,]=xx$weights.ageSurf[id.allyears,]
        spweights= weights[5,]
        Fs = xx$Fs[rows,3:33]
        maxF = apply(Fs,1,max)

        phaseF = rep(-1,6)
        Fini=rep(0,6)
        phaseF[id] =1
        Fini[id] = maxF[id]
        Fini[Fini>0.20]=0.20
        write(Fini,filename,append=T,ncolumns=6)
        write(phaseF,filename,append=T,ncolumns=6)
        write("# sel",filename,append=T)
        write(t(sel),filename,append=T,ncolumns=31)
        write("# weights",filename,append=T)
        write(t(weights),filename,append=T,ncolumns=31)
        write(spweights,filename,append=T,ncolumns=31)
        write(xx$M,filename,append=T,ncolumns=31) 
        write(catch.split,filename,append=T,ncolumns=6) 
        write(c(xx$alpha[2],xx$beta[2],1),filename,append=T,ncolumns=3) 
     }       
   }
}
