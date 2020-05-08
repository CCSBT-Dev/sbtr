#' @title Table of biomass f
#' @description
#' To produce a table of one of the percentiles (e.g. 10th, median, 90th) of biomass in a particular year. for a range of MPs and catch schedules.
#' @export
#' 
table.biomass.f <- function(dr=c("CONST"), ver=c("c0s0l0", "c0s0l1"), model=c(0),quants=c(.1,.5,.9),relative.to=2004,
  yr.required=2014, path="local", B.option="rel.to.single")
{
#TAB: adjusted Sept 2006 to look at the values for 
#TA Branch starting 15 July 2005 based on cmp.worms.f by Jason Hartog and Paige. 

#Arguments to function are:
#1) MPs: list of MPs, e.g. c("CONST", "CMP_1", "CMP_2")
#2) sched: list of schedules to compare over, e.g. c("2b","2b2500", "3d")
#3) model: the model to compare, e.g. "refset"
#4) quants: the percentile to calculate, e.g. 0.5 for median
#5) yr.required: the future year to calculate for, e.g. 2014
#6) path: the directory where the tree starts
#6) relative.to: year to divide by. If -1 then does not divide by anything
#7) B.option: should the biomass be relative to the 2004 value for that single run ("rel.to.single") or 
#     relative to the median 2004 value over all runs ("rel.to.median"), or just the biomass in yr.required: ("biomass.only").

	.Options$warn <- -1  # suppress annoying warning messages
	
	path.MP <- path
	
	nver <- length(ver)
	nmodel <- length(model)
	first.time <- 1
	
	for (k in 1:nver) {
		for(i in 1:nmodel){ 
		  path <- paste(path.MP,"\\",dr,"\\CR",model[i],"\\",sep="")
		  wormfile <-paste(path,paste(dr,ver[k],model[i],sep="_"),".s4",sep="")
		  tmp <- read.table(wormfile,skip=3)
		  scn <- tmp[,2]
		  tmp <- as.matrix(tmp[,-c(1,2)])
		  yr.first <- 1931
		  yr.last <- yr.required
		  nyr <- yr.required - yr.first
		  
		  B.relto.yr <- tmp[,relative.to - yr.first+1]
		  B.required.yr <- tmp[,yr.required - yr.first+1]
		
		  if(B.option=="rel.to.single") bworms<- B.required.yr/B.relto.yr
		  if(B.option=="rel.to.median") bworms<- B.required.yr/quantile(B.relto.yr,0.5)
		  if(B.option=="biomass.only") bworms<- B.required.yr
		
		  
		  if(first.time==1) {
		    B.quantiles<- array(data=0,dim=c(nver,nmodel,length(quants)),dimnames=list(ver,model,quants))
		    first.time<- -1
		  }
		  for(j in 1:length(quants)){
		    B.quantiles[k,i,j]<- quantile(bworms,quants[j])
		  }  #end for j
		} #end for i
	} #end for k
	return(B.quant.in.year=B.quantiles)
}
  
