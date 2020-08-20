wiggle <- function(s3file, start.yr, end.yr, quants=c(0.5)) {
	# get future TACs from .s3 file and calculate wiggle factor
	# ::
	# W = sum_years((d2TAC/dt2)^2 = (TAC[y+1]-2TAC[y]+TAC[y-1])^2)
	# basically the smoothness penalty you put on a GAM to avoid over-fitting

   tmp <- s3file
   col.start <- grep(paste("C.",start.yr,sep=""),colnames(tmp))
   col.end <- grep(paste("C.",end.yr,sep=""),colnames(tmp))
   cmat <- tmp[,col.start:col.end]

	# get rid of scale issue and mean standardise each catch trajectory

	cmat.ms <- t(apply(cmat,1,function(x){x <- x/mean(x)}))
	xx <- (cmat.ms[,-c(1,2)]-2*cmat.ms[,-c(1,dim(cmat.ms)[2])]+cmat.ms[,-c(dim(cmat.ms)[2]-1,dim(cmat.ms)[2])])^2
	W <- apply(xx,1,sum)

	# W is the statistic (value unimportant so no need for scale)
   temp <- quantile(W,quants)
	invisible(temp)
}
