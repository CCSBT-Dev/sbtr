######################################################
# statistical summary of MPs #########################
######################################################
# R Hillary CSIRO 2018 ###############################
######################################################

library(ggplot2)

# generate the results
dirnm <- "../outputs" # put the directory name where the .s* files are here

# list of grids
grd <- c("base16")

# list of tuning objectives (25, 30, 35, 40)
#tun <- as.character(c(25,30,35,40))
tun <- as.character(c(30))

# list of MPs
#mplist <- c("rh1","rh2","rh3","rh4","rh5","rh7","rh8")
mplist <- c("DMM3_3000")

# reference/robustness trials
rlist <- c("ref")

# MC iterations
nits <- 2000

## Summary stat names
# TRO
trolist <- c("Old objective","2035 vs 2017","2040 vs 2035","Trend 2021-2035")

# TAC
taclist <- c("Mean TAC","AAV","Max. decr.","P(2up/1down)","10%ile TAC")

# create the data frames
trodf <- expand.grid(grid=grd,tuning=tun,MP=mplist,stat=trolist,run=rlist,iter=1:nits,val=NA)
tacdf <- expand.grid(grid=grd,tuning=tun,MP=mplist,stat=taclist,run=rlist,iter=1:nits,val=NA)

tacyr <- as.character(2017:2045)
troyr <- as.character(1931:2046)
decyr <- as.character(c(2021,2024,2027,2030,2033))

for (g in grd) {
  for (t in tun) {
    for (m in mplist) {
      for (r in rlist) {

      fnm <- paste(dirnm, m, sep = "/")
      #fnm <- paste(fnm, m, sep = "_")
      fnm <- paste(fnm, t, sep = "_")
      if (r == 'ref') fnm <- fnm
      if (r != 'ref') fnm <- paste(fnm, r, sep = "_")
      fnm <- paste(fnm, g, sep = "_")
      s4nm <- paste(fnm, "s4", sep = ".")
      s9nm <- paste(fnm, "s9", sep = ".")
      
      s4df <- read.table(s4nm, skip = 2, header = TRUE)
      s9df <- read.table(s9nm, skip = 1, header = TRUE)
      
      TRO <- as.matrix(s4df[,grep("SSB", names(s4df))])   
      TAC <- as.matrix(s9df[,grep("TAC", names(s9df))])
      colnames(TRO) <- troyr
      colnames(TAC) <- tacyr

      # TRO stats:
      # 1. P(2035 > 0.2 B0)
      # 2. P(2035 > 2017)
      # 3. P(2040 > 2035)
      # 4. Log-linear trend (2021 to 2035)

      snm <- trolist[1]
      xx <- TRO[,as.character(2035)]/(0.2*TRO[,1])
      trodf[trodf$grid==g & trodf$tuning==t & trodf$MP==m & trodf$run==r & trodf$stat== snm,"val"] <- length(xx[xx>1])/nits

      snm <- trolist[2]
      xx <- TRO[,as.character(2035)]/TRO[,as.character(2017)]
      trodf[trodf$grid==g & trodf$tuning==t & trodf$MP==m & trodf$run==r & trodf$stat== snm,"val"] <- length(xx[xx>1])/nits

      snm <- trolist[3]
      xx <- TRO[,as.character(2040)]/TRO[,as.character(2035)]
      trodf[trodf$grid==g & trodf$tuning==t & trodf$MP==m & trodf$run==r & trodf$stat== snm,"val"] <- length(xx[xx>1])/nits

      snm <- trolist[4]  
      xx <- log(TRO[,as.character(2021:2035)])
      yy <- 2021:2035
      lam <- rep(NA,nits)
      for(n in 1:nits) lam[n] <- sum((xx[n,]-mean(xx[n,]))*(yy-mean(yy)))/sum((yy-mean(yy))^2)
      trodf[trodf$grid==g & trodf$tuning==t & trodf$MP==m & trodf$run==r & trodf$stat== snm,"val"] <- lam
  
      # TAC stats:
      # 1. Mean TAC (2021 to 2035)
      # 2. AAV (2021 to 2035)
      # 3. Max TAC decrease (2021 to 2035)
      # 4. P(TAC[r+3]<TAC[r+2]) IF TAC[r+1]>TAC[r] & TAC[r+2]>TAC[r+1] r = 1
      # 5. 10%ile TAC (2021 to 2035)

      snm <- taclist[1]
      xx <- TAC[,as.character(2021:2035)]
      tacdf[tacdf$grid==g & tacdf$tuning==t & tacdf$MP==m & tacdf$run==r & tacdf$stat== snm,"val"] <- apply(xx,1,mean)

      snm <- taclist[2]
      xx <- TAC[,decyr]
      zz <- abs(100*(xx[,-c(1)]/xx[,-c(dim(xx)[2])]-1))
      tacdf[tacdf$grid==g & tacdf$tuning==t & tacdf$MP==m & tacdf$run==r & tacdf$stat== snm,"val"] <- apply(zz,1,mean)
      
      snm <- taclist[3] 
      xx <- TAC[,decyr]
      zz <- xx[,-c(1)]-xx[,-c(dim(xx)[2])]
      del <- rep(NA,nits) 
      for(n in 1:nits) del[n] <- ifelse(any(zz[n,]<0),-min(zz[n,]),0)
      tacdf[tacdf$grid==g & tacdf$tuning==t & tacdf$MP==m & tacdf$run==r  & tacdf$stat== snm,"val"] <- del

      snm <- taclist[4] 
      xx <- TAC[,decyr]
      zz <- xx[,-c(1)]-xx[,-c(dim(xx)[2])] 
      ud <- rep(NA,nits)
      cnt1 <- 0
      cnt2 <- 0
      for(n in 1:nits) {

        if(zz[n,1] > 0 & zz[n,2] > 0) {

          cnt1 <- cnt1+1
          if(zz[n,3] < 0) cnt2 <- cnt2+1

        }
      }
      if(cnt1 == 0 & cnt2 == 0) pp <- 0
      if(cnt1 > 0 & cnt2 == 0) pp <- 0
      if(cnt1 > 0 & cnt2 > 0) pp <- cnt2/cnt1
      tacdf[tacdf$grid==g & tacdf$tuning==t & tacdf$MP==m & tacdf$run==r & tacdf$stat== snm,"val"] <- rbeta(nits,0.5+cnt2,0.5+cnt1)
 
      snm <- taclist[5] 
      xx <- TAC[,as.character(2021:2035)]
      tacdf[tacdf$grid==g & tacdf$tuning==t & tacdf$MP==m & tacdf$run==r  & tacdf$stat== snm,"val"] <- mean(apply(xx,2,quantile,0.1))
  
      }
    }
  }
}

## TRO plots

# Probabilistic ones

pp1 <- ggplot(subset(trodf,stat != trolist[4]),aes(y=val,x=MP))+geom_point()+ylim(0,1)+ylab("Probability")
pp1 <- pp1+facet_grid(tuning~stat)
pp1

# trend plot

pp2 <- ggplot(subset(trodf,stat == trolist[4]),aes(y=val,x=MP,fill=MP))+geom_boxplot(outlier.shape=NA)+ylab(expression(lambda[2021-2035]^{TRO}))+ylim(c(-0.075,0.12))
pp2+facet_grid(. ~ tuning)

## TAC plots

# Catch-based ones first

pp3 <- ggplot(subset(tacdf,stat != taclist[2] & stat != taclist[4]),aes(y=val,x=MP,fill=MP))+geom_boxplot(outlier.shape=NA)+ylab("Catch stats.")
pp3+facet_grid(stat~tuning,scales='free_y')

# AAV

pp4 <- ggplot(subset(tacdf,stat == 'AAV'),aes(y=val,x=MP,fill=MP))+geom_boxplot(outlier.shape=NA)+ylab("Percentage")
pp4+facet_grid(.~tuning)

# P(C up then down)

pp5 <- ggplot(subset(tacdf,stat == taclist[4]),aes(y=val,x=MP,fill=MP))+geom_boxplot(outlier.shape=NA)+ylim(c(0,1))+ylab(taclist[4])
pp5+facet_grid(.~tuning)

