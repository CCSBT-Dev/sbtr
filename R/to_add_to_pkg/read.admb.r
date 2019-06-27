#' Read standard ADMB output files
#' 
#' @param file is the root name of the ADMB model 
#' @return a list of objects
#' @author Anders Nielsen
#' @examples
#' sbtmodres <- read.admb(file = "sbtmod")
#' @export
#' @seealso read.fit
#' \code{\link{read.fit}}, and 
#' \code{\link{read.rep}} 
#'
read.admb <- function(ifile)
{	
	ret=read.fit(ifile)
	fn=paste(ifile,'.rep', sep='')
	A=read.rep(fn)
	A$fit=ret
	
	pfn=paste(ifile,'.psv',sep='')
	if(file.exists(pfn))
		A$post.samp=read.psv(pfn)
	
	return(A)
}


#' read.fit
#' 
#' @param file is the root name of the ADMB model 
#' @return a list of objects
#' @author Anders Nielsen
#' @examples
#' sbtmodres <- read.fit(file = "sbtmod")
#' @export
#' @seealso 
#' \code{\link{read.rep}}, and 
#' \code{\link{read.admb}} 
read.fit <- function(ifile)
{
	ret<-list() 
	parfile<-as.numeric(scan(paste(ifile,'.par', sep=''),   
	 what='', n=16, quiet=TRUE)[c(6,11,16)]) 
	ret$nopar<-as.integer(parfile[1]) 
	ret$nlogl<-parfile[2] 
	ret$maxgrad<-parfile[3] 
	file<-paste(ifile,'.cor', sep='') 
	lin<-readLines(file) 
	ret$npar<-length(lin)-2 
	ret$logDetHess<-as.numeric(strsplit(lin[1], '=')[[1]][2]) 
	sublin<-lapply(strsplit(lin[1:ret$npar+2], ' '),function(x)x[x!='']) 
	ret$names<-unlist(lapply(sublin,function(x)x[2])) 
	ret$est<-as.numeric(unlist(lapply(sublin,function(x)x[3]))) 
	ret$std<-as.numeric(unlist(lapply(sublin,function(x)x[4]))) 
	ret$cor<-matrix(NA, ret$npar, ret$npar) 
	corvec<-unlist(sapply(1:length(sublin), function(i)sublin[[i]][5:(4+i)])) 
	ret$cor[upper.tri(ret$cor, diag=TRUE)]<-as.numeric(corvec) 
	ret$cor[lower.tri(ret$cor)] <- t(ret$cor)[lower.tri(ret$cor)] 
	ret$cov<-ret$cor*(ret$std%o%ret$std)
	return(ret)
}


#' Reads an ADMB report file
#'
#' The following reads a report file
#' 
#' @param file is the root name of the ADMB model 
#' @return a list of objects
#' @author Anders Nielsen
#' @examples
#' sbtmodres <- read.rep(file = "sbtmod")
#' @export
#' @seealso 
#' \code{\link{read.fit}}, and 
#' \code{\link{read.admb}} 
read.rep <- function(fn)
{
	# Then the 'A' object contains a list structure
	# with all the elemements in the report file.
	# In the REPORT_SECTION of the AMDB template use 
	# the following format to output objects:
	#  	report<<"object \n"<<object<<endl;
	#
	# The part in quotations becomes the list name.
	# Created By Steven Martell
	options(warn=-1)  #Suppress the NA message in the coercion to double
	
	
	ifile=scan(fn,what="character",flush=TRUE,blank.lines.skip=FALSE,quiet=TRUE)
	idx=sapply(as.double(ifile),is.na)
	vnam=ifile[idx] #list names
	nv=length(vnam) #number of objects
	A=list()
	ir=0
	for(i in 1:nv)
	{
		ir=match(vnam[i],ifile)
		if(i!=nv) irr=match(vnam[i+1],ifile) else irr=length(ifile)+1 #next row
		dum=NA
		if(irr-ir==2) dum=as.double(scan(fn,skip=ir,nlines=1,quiet=TRUE,what=""))
		if(irr-ir>2) dum=as.matrix(read.table(fn,skip=ir,nrow=irr-ir-1,fill=TRUE))

		if(is.numeric(dum))#Logical test to ensure dealing with numbers
		{
			A[[vnam[i]]]=dum
		}
	}
	options(warn=0)
	
	return(A)
}


#' Reads an ADMB dat file
#'
#' The following reads a report file
#' 
read.dat <- function(fn)
{
    # The following reads a report file
    # Then the 'A' object contains a list structure
    # with all the elemements in the report file.
    # In the REPORT_SECTION of the AMDB template use 
    # the following format to output objects:
    #  	report<<"object \n"<<object<<endl;
    #
    # The part in quotations becomes the list name.
    # Created By Steven Martell
    options(warn=-1)  #Suppress the NA message in the coercion to double
    ifile=scan(fn,what="character",flush=TRUE,blank.lines.skip=FALSE,quiet=TRUE)
    idx=sapply(as.double(ifile),is.na)
    
    ifile[idx]=substr(ifile[idx],2,20) #list names but strip first character (# sign)
    vnam=ifile[idx] #list names but strip first character (# sign)
    nv=length(vnam) #number of objects
    print(vnam)
    A=list()
    ir=0
    for(i in 1:nv)
    {
      ir=match(vnam[i],ifile)
      if(i!=nv) irr=match(vnam[i+1],ifile) else irr=length(ifile)+1 #next row
      dum=NA
      if(irr-ir==2) dum=as.double(scan(fn,skip=ir,nlines=1,quiet=TRUE,what=""))
      if(irr-ir>2) dum=as.matrix(read.table(fn,skip=ir,nrow=irr-ir-1,fill=TRUE))
      
      if(is.numeric(dum))#Logical test to ensure dealing with numbers
      {
        A[[vnam[i]]]=dum
      }
    }
    options(warn=0)
    
    return(A)
}


#' Read a binary output from ADMB
#' 
#' This function reads the binary output from ADMB
#' 
#' @param file is the root name of the ADMB model 
#' @return a matrix of MCMC parameter vectors that is saved via "-mceval"
#' @author Anders Nielsen
#' @examples
#' sbtmodres <- read.fit(file = "sbtmod")
#' @export
#' @seealso 
#' \code{\link{read.rep}}, and 
#' \code{\link{read.admb}} 
read.psv <- function(fn, nsamples=10000)
{
	#-mcsave command line option.
	#fn = paste(ifile,'.psv',sep='')
	filen <- file(fn, "rb")
	nopar <- readBin(filen, what = integer(), n = 1)
	mcmc <- readBin(filen, what = numeric(), n = nopar * nsamples)
	mcmc <- matrix(mcmc, byrow = TRUE, ncol = nopar)
	close(filen)
	return(mcmc)
}
