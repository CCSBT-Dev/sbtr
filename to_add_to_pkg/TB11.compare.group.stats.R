 print.title <- function(xrel,yrel,title,cex=0.8) {
    coords <- par("usr")
    text(x=coords[1]+xrel*diff(coords[1:2]), y=coords[3]+yrel*diff(coords[3:4]),title, pos=4)
 }
 ##print.title(x=0,y=1,"HELP")

##############################################################################
#Plot just one of the statistics
##############################################################################
plot.one.stat <- function(plot.data, plot.title, abline.at=0, ngroups, nelements, group.labs) {
     #for positioning of elements within groups
    if (nelements == 1) POS <- c(0)
    if (nelements == 2) POS <- c(-0.1,0.1)
    if (nelements == 3) POS <- c(-0.2,0,0.2)
    if (nelements == 4) POS <- c(-0.225,-0.075,0.075,0.225)
    if (nelements == 5) POS <- c(-0.26,-0.13,0,0.13,0.26)
    if (nelements == 6) POS <- c(-0.28,-0.17,-0.06,0.06,0.17,0.28)

    ###Plotting parameters, general
     title.ypos <- 0.9
     title.xpos <- 0
     xlim <- c(0.5,ngroups+0.5)
     pch <- 21
     require(gplots)
     #pt.col <- rainbow(nelements+1)
     pt.col <- rep("black",nelements)
     bg.col <- rich.colors(nelements+2)[2:(nelements+1)]
     error.col <- rep("gray30",nelements)
     pt.cex <- 1.8
     bar.lwd <- 1.8
     
     #get the figure set up correctly (wasteful I know but then can do arrows first so they don't overlay points
     #probably a way of setting up new figure without actually plotting anything
     plot(x=1,y=1,type="n",xlim=xlim, ylim=c(0,1.3*max(plot.data,abline.at)), axes=F)
     if (abline.at>0) {
         abline(h=abline.at,lty=2,col="gray50")
     }
     for (j in 1:nelements) {
        xvals <-1:ngroups+POS[j] 
        par(new=T)
        arrows(x0=xvals,x1=xvals,y0=plot.data[,j,1],y1=plot.data[,j,3],length=0,col=error.col[j],lwd=bar.lwd)
        par(new=T)
        plot(x=xvals,plot.data[,j,2],type="p",xlim=xlim, ylim=c(0,1.3*max(plot.data,abline.at)), axes=F,pch=pch, 
                              bg=bg.col[j], col=pt.col[j], cex=pt.cex)
        if(!missing(group.labs)) {
            axis(side=1,at=1:ngroups,lab=group.labs,las=2, cex.axis=1.05)
        }
     }
     axis(2)
     box()
     print.title(x=title.xpos,y=title.ypos,plot.title)
     
 
 }


 ##############################################################################
 #compare.group.stats
 #Written by T.A. Branch starting Jan 27, 2010. 
 #Borrowing code ideas written by a variety of CSIRO scientists from 2001-2005.
 #Inputs
 #1. A list with groups of file prefixes (i.e. exclude the .s4 but include directories)
 #     each group will be plotted together, and constrasted with other groups, e.g.
 #     file.pre <- list(c("const0_c1s1l1","const10000_c1s1l1"), c("const0_c1s1l13h","const10000_c1s1l13h") )
 #     in this example the c1s1l1 cases would be close to each other and separated from c1s1l13h cases.
 #
 #Definition of statistics:
 #1. C 10yr avg = average 10 yr catch from first ten years of future catches
 #
 ##############################################################################
 
 #1. Need to change AAV to start in different year
 
 compare.group.stats <- function(file.prefixes, yr.now=2018, yr.longterm=2045, zero.prefix="CONST\\v0\\CONST0_", 
                                 group.labs, element.labs, MP.yrs = c(2018, 2045), quants=c(0.1,0.5,0.9)) {
    par(mfcol=c(5,2), mai=c(0,.3,.13,.1),omi=c(.9,.3,.3,0), las=1, cex.axis=0.8, xaxs="i", yaxs="i")
    
    ngroups <- length(file.prefixes)         #how many groups to compare (e.g. CMPs)
    nelements <- length(file.prefixes[[1]])  #how many elements within the groups, assume same within groups
    nquants <- length(quants)
    
    #Find the current year (so don't have to redo this for every CCSBT year)
    tmp.names <- colnames(read.table(paste(file.prefixes[[1]][1],".s3",sep=""),skip=2,header=T, nrows=2))
    current.yr <- yr.now
    while (length(grep(paste("C.",current.yr,sep=""), tmp.names))<1) {
       current.yr <- current.yr + 1
    }
    
    ###arrays to store the calculated results
    ave.10.catches <- array(data=NA, dim=c(ngroups,nelements,nquants),dimnames=list(group.labs,element.labs,paste("Quantile",quants)))
    ave.20.catches <- ave.10.catches
    AAV <- ave.10.catches
    TAC.dec <- ave.10.catches
    C.wiggle <- ave.10.catches
    B.depletion <- ave.10.catches
    B.5yr <- ave.10.catches
    B.longterm <- ave.10.catches
    B.min.current <- ave.10.catches
    B.longterm.B.star <- ave.10.catches
    CPUE.5yr <- ave.10.catches
    
    source("R/TB11.wiggle v3.R")    
    ###
    for (i in 1:ngroups) {
       for (j in 1:nelements) {
           s3file <- read.table(paste(file.prefixes[[i]][j],".s3",sep=""),skip=2,header=T)
           s4file <- read.table(paste(file.prefixes[[i]][j],".s4",sep=""),skip=2,header=T)
           #zero.filename <- paste(zero.prefix, (strsplit(file.prefixes[[i]][j],"_"))[[1]][2], ".s4", sep="")
           #zerofile <- read.table(zero.filename,skip=2,header=T)
           
           catch.10.cols <- c(grep(paste("C.",current.yr,sep=""),colnames(s3file)), grep(paste("C.",current.yr+9,sep=""),colnames(s3file)))
           ave.10.catches[i,j,] <- quantile(rowMeans(s3file[,catch.10.cols[1]:catch.10.cols[2]]),quants)
           catch.20.cols <- c(grep(paste("C.",current.yr,sep=""),colnames(s3file)), grep(paste("C.",current.yr+19,sep=""),colnames(s3file)))
           ave.20.catches[i,j,] <- quantile(rowMeans(s3file[,catch.20.cols[1]:catch.20.cols[2]]),quants)
           catch.change.cols <- c(grep(paste("C.", MP.yrs[1] - 1, sep = ""), colnames(s3file)), grep(paste("C.",MP.yrs[2],sep=""),colnames(s3file)))
           catch.change.cols <- catch.change.cols[1]:catch.change.cols[2] 
           nAAV <- length(catch.change.cols)
           C.diffs <- s3file[,catch.change.cols[-1]]-s3file[,catch.change.cols[-nAAV]]
           AAV[i,j,] <- quantile(rowSums(abs(C.diffs))/(nAAV-1),quants)
           TAC.dec[i,j,] <- quantile(-1*apply(C.diffs,MARGIN=1,min),quants)
           
           YY <- wiggle(s3file=s3file, MP.yrs[1]-1, MP.yrs[2], quants=quants)
           C.wiggle[i,j,] <- YY
           #print(YY)
           #XX <- (      s3file[,catch.change.cols[-c(1,2)]] 
           #         - 2*s3file[,catch.change.cols[-c(1,nAAV)]] 
           #         + s3file[,catch.change.cols[-c(nAAV-1,nAAV)]] )^2
           #print(quantile(apply(XX,1,sum),quants))
           #C.wiggle[i,j,] <- quantile(apply(XX,1,sum),quants)

           Depl.cols <- c(grep("B.1931",colnames(s4file)),grep(paste("B.",yr.longterm,sep=""),colnames(s4file)) )
           B.depletion[i,j,] <- quantile(s4file[,Depl.cols[2]]/s4file[,Depl.cols[1]], quants)
           Five.cols <- c(grep(paste("B.",current.yr,sep=""),colnames(s4file)),grep(paste("B.",current.yr+5,sep=""),colnames(s4file)) )
           B.5yr[i,j,] <- quantile(s4file[,Five.cols[2]]/s4file[,Five.cols[1]], quants)
           Blongterm.cols <- c(grep(paste("B.",current.yr,sep=""),colnames(s4file)),grep(paste("B.",yr.longterm,sep=""),colnames(s4file)) )
           B.longterm[i,j,] <- quantile(s4file[,Blongterm.cols[2]]/s4file[,Blongterm.cols[1]], quants)
           
           #Note that biomass reported for one year after last MP year
           Bmin.cols <- c(grep(paste("B.",current.yr,sep=""),colnames(s4file)),grep(paste("B.",MP.yrs[2]+1,sep=""),colnames(s4file)) )
           B.min.current[i,j,] <- quantile(apply(s4file[,Bmin.cols[1]:Bmin.cols[2]]/s4file[,Bmin.cols[1]],MARGIN=1,min), quants)
           
           #Bstar.cols <- grep(paste("B.",yr.longterm,sep=""),colnames(zerofile))
           #B.longterm.B.star[i,j,] <- quantile(s4file[,Blongterm.cols[2]]/zerofile[,Bstar.cols], quants)

           CPUE.cols <- c(grep(paste("CPUE.",current.yr,sep=""),colnames(s4file)),grep(paste("CPUE.",current.yr+5,sep=""),colnames(s4file)) )
           CPUE.5yr[i,j,] <- quantile(s4file[,CPUE.cols[2]]/s4file[,CPUE.cols[1]], quants)

       }
    }
    
    
    ###Plotting parameters, general
    title.ypos <- 0.9
    title.xpos <- 0
    xlim <- c(0.5,ngroups+0.5)
    pch <- 21
    require(gplots)
    #pt.col <- rainbow(nelements+1)
    pt.col <- rep("black",nelements)
    bg.col <- rich.colors(nelements+2)[2:(nelements+1)]
    error.col <- rep("gray30",nelements)
    pt.cex <- 1.8
    bar.lwd <- 1.8
    
    ###plot each of the elements
    plot.one.stat(plot.data=ave.10.catches, plot.title=paste("Mean catch ", current.yr,"-", current.yr+9,sep=""), 
                  ngroups=ngroups, nelements=nelements) 
    par(xpd=NA)    #allows legend to be plotted outside the coordinates of the plot
    coords <- par("usr")   #returns current coordinates of the top left plot
    legend(x=coords[1],y=coords[4]+0.33*diff(coords[3:4]),legend=element.labs,bty="n", col=pt.col,pch=pch,pt.bg=bg.col, pt.cex=pt.cex*1.3, ncol=nelements)
    par(xpd=F)     #restores plotting to be within the plot bounds again
    plot.one.stat(plot.data=ave.20.catches, plot.title=paste("Mean catch ", current.yr,"-", current.yr+19,sep=""), 
                  ngroups=ngroups, nelements=nelements) 
    plot.one.stat(plot.data=AAV, plot.title=paste("AAV ", MP.yrs[1]-1,"-", MP.yrs[2],sep=""), 
                  ngroups=ngroups, nelements=nelements) 
    plot.one.stat(plot.data=TAC.dec, plot.title=paste("Maximum TAC decrease ", MP.yrs[1]-1,"-", MP.yrs[2],sep=""), abline.at=5000, 
                  ngroups=ngroups, nelements=nelements) 
    plot.one.stat(plot.data=C.wiggle, plot.title=paste("C wiggle ", MP.yrs[1]-1,"-", MP.yrs[2],sep=""), abline.at=1, 
                  ngroups=ngroups, nelements=nelements,group.labs=group.labs) 
    plot.one.stat(plot.data=B.depletion, plot.title=expression(SSB[2025]/SSB[0]), abline.at=0.2, 
                  ngroups=ngroups, nelements=nelements) 
    plot.one.stat(plot.data=B.5yr, plot.title=paste("SSB[",current.yr+5,"]/SSB[",current.yr,"]",sep=""), abline.at=1, 
                  ngroups=ngroups, nelements=nelements) 
    plot.one.stat(plot.data=B.longterm, plot.title=paste("SSB[",2025,"]/SSB[",current.yr,"]",sep=""), abline.at=1, 
                  ngroups=ngroups, nelements=nelements) 
    plot.one.stat(plot.data=B.min.current, plot.title=paste("SSBmin/SSB[",current.yr,"]",sep=""), abline.at=1, 
                  ngroups=ngroups, nelements=nelements) 
    #plot.one.stat(plot.data=B.longterm.B.star, plot.title=expression(paste(B[2025]/B[2025]^"star")), abline.at=1, 
    #              ngroups=ngroups, nelements=nelements,group.labs=group.labs) 
    plot.one.stat(plot.data=CPUE.5yr, plot.title=paste("CPUE[",current.yr+5,"]/CPUE[",current.yr,"]",sep=""), abline.at=1, 
                  ngroups=ngroups, nelements=nelements, group.labs=group.labs) 

    invisible(list(ave.10.catches=ave.10.catches, ave.20.catches=ave.20.catches, AAV=AAV, TAC.dec=TAC.dec, C.wiggle=C.wiggle, 
                B.depletion=B.depletion, B.5yr=B.5yr, B.longterm=B.longterm, B.min.current=B.min.current, 
                B.longterm.B.star=B.longterm.B.star, CPUE.5yr=CPUE.5yr))
 }
 
 #source("TB10.compare.group.stats.r")
 #win.graph(width=8.5, height=11,pointsize=12)
 #pdf("figs\\CompareStats.pdf",width=8.5,height=11)
 
 #====Shortest possible run
 #catch.levels <- 10000
 #grid.files <- c("c1s1l13h")
 #file.prefixes <- list(paste("CONST\\v0\\CONST",catch.levels,"_",grid.files,sep=""))
 
 #====Long run with multiple options 
 #catch.levels <- seq(0,10000,2000)
 #grid.files <- c("c1s1l13h","c1s1l1","c0s1l13h","c2s1l13h","c1s1l23h","c1s0l13h")
 #file.prefixes <- list(paste("CONST\\v0\\CONST",catch.levels,"_",grid.files[1],sep=""),
 #                      paste("CONST\\v0\\CONST",catch.levels,"_",grid.files[2],sep=""),
 #                      paste("CONST\\v0\\CONST",catch.levels,"_",grid.files[3],sep=""),
 #                      paste("CONST\\v0\\CONST",catch.levels,"_",grid.files[4],sep=""),
 #                      paste("CONST\\v0\\CONST",catch.levels,"_",grid.files[5],sep=""),
 #                      paste("CONST\\v0\\CONST",catch.levels,"_",grid.files[6],sep=""))
 #x <- compare.group.stats(file.prefixes=file.prefixes, zero.prefix="CONST\\v0\\CONST0_", group.labs=grid.files, element.labs=paste(catch.levels,"mt"))
 #dev.off()