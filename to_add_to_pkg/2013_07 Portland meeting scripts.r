#===============================================================
#List of r code used in Portland ME meeting of CCSBT
#23-26 July 2013.
#Written by Trevor A. Branch tbranch@gmail.com / tbranch@uw.edu
#===============================================================

#=====Required packages==============
#install.packages("PBSmodelling")
require(PBSmodelling)

####################################################################
#Read in all data from the .lab rep files. 
source("get.all.files.r")
#data.base <- get.all.files("arc\\base2010sqrt")
#data.base.AC <- get.all.files("arc\\base2010sqrtAC")
#data.base.noCK <- get.all.files("arc\\base2010noCK")
#data.base7h <- get.all.files("arc\\base7hsqrt")

#data.base6M <- get.all.files("arc\\base6Msqrt") #final base case
#data.base.AC12 <- get.all.files("arc\\base2012sqrtAC")  #final case with AC=0.5

#data.c1lambda1 <- get.all.files("arc\\base2012sqrt_c1lambda1")
#data.c2lambda1 <- get.all.files("arc\\base2012sqrt_c2lambda1")
#data.c3lambda1 <- get.all.files("arc\\base2012sqrt_c3lambda1")
#data.c4lambda1 <- get.all.files("arc\\base2012sqrt_c4lambda1")
#data.c4lambda25 <- get.all.files("arc\\base2012sqrt_c4lambda25")
#data.c4lambda50 <- get.all.files("arc\\base2012sqrt_c4lambda50")
#data.c4lambda75 <- get.all.files("arc\\base2012sqrt_c4lambda75")

#data.base.NS <- get.all.files("arc\\base2010sqrtns")
#data.base.NS_emature <- get.all.files("arc\\base2010sqrtns_emature")
#data.base.NS_lnq <- get.all.files("arc\\base2010sqrtns_lnq")

#data.midcellNewSel <- get.all.files("arc\\midcellNewSel")
#data.midcellsqrt <- get.all.files("arc\\midcellsqrt")
#data.baseCKmk1 <- get.all.files("arc\\baseCKmk1sqrt")
#data.baseCKmk5 <- get.all.files("arc\\baseCKmk5sqrt")

example.labrep <- "arc\\base2010sqrt\\base2010sqrt_h2m3M3O2C2a1_lab.rep"
example.obj <- data.base[["base2010sqrt_h2m3M3O2C2a1_lab.rep"]]
label <- "h2m3M3O2C2a1"

#***Figure 1
#Likelihood plot comparison between base grid and the other grid
source("TableLikelihoodComponents v2.r")
nll.table.base  <- likelihood.table2(data.objects = data.base)
nll.table.baseCKmk1  <- likelihood.table2(data.objects = data.baseCKmk1)
nll.table.baseCKmk5  <- likelihood.table2(data.objects = data.baseCKmk5)

source("PlotNLLComponents.r")
pdf("figs\\Fig 1 NLLs base.pdf") 
plot.NLL.by.steepness(nll.table.base,caption="base2010sqrt")
plot.NLL.by.steepness(nll.table.baseCKmk5,caption="baseCKmk5sqrt")
plot.NLL.by.steepness(nll.table.baseCKmk1,caption="baseCKmk1sqrt")
dev.off()

#***Figure 2
#Plot the MSY, Fmsy, Bmsy (final year, can also do in every year)
#source("get.all.files.r")
#data.base <- get.all.files("arc\\base2010sqrt")
source("MSY values.r")
pdf(file="figs\\Fig 2 MSY.pdf",width=10,height=8)
x <- MSY.vals(data.objects=data.base,label="")
dev.off()

#***Fig 3 Shaded plot base
#make a 150KB file instead of 1.5 MB file
source("ShadedPlots.r")
jpeg(file="figs\\Fig 3 Shaded base2010sqrt.jpg", width=600,height=600)
plot.lev("levfiles\\base2010sqrt.lev")
dev.off()
jpeg(file="figs\\Fig 3 Shaded basenoCK.jpg", width=600,height=600)
plot.lev("levfiles\\basenoCK.lev")
dev.off()


#***Figure 4 Aussie age fits
source("AussieAgeFits.r")
pdf("figs\\Fig 4 AussieAge.pdf",width=6, height=11.5)
AussieAgeFits(labrep.file=example.labrep,case_label="example")
dev.off()  #to close pdf file, when errors occur type this in a few times. 

#***Figure 5 Indo age fits
source("IndoAgeFits.r")
pdf("figs\\Fig 5 IndonesianAge.pdf",width=6, height=11.5)
IndoAgeFits(data.object=example.obj,case_label=label)
dev.off()

###Fig 6-9 Length fits to LL1, LL2, LL3, LL4
#example.labrep <- "arc\\base2010sqrt\\base2010sqrt_h2m3M3O2C2a1_lab.rep"
label <- "h2m3M3O2C2a1"

source("LengthsLL1.r")
pdf("figs\\Fig 6 LengthsLL1.pdf",width=8, height=11.5)
LengthsLL1(labrep.file=example.labrep,case_label=label)
dev.off()

pdf("figs\\Fig 6 LengthsLL1 midcell.pdf",width=8, height=11.5)
example.labrep <- "arc\\midcellsqrt\\midcellsqrt_h2m3M3O2C2a1_lab.rep"
LengthsLL1(labrep.file=example.labrep,case_label="midcellsqrt")
example.labrep <- "arc\\midcellNewSel\\midcellnewsel_h2m3M3O2C2a1_lab.rep"
LengthsLL1(labrep.file=example.labrep,case_label="midcellNewSel")
dev.off()


source("LengthsLL2.r")
pdf("figs\\Fig 7 LengthsLL2.pdf",width=6, height=8)
LengthsLL2(labrep.file=example.labrep,case_label=label)
dev.off()

source("LengthsLL3.r")
pdf("figs\\Fig 8 LengthsLL3.pdf",width=8, height=11.5)
LengthsLL3(labrep.file=example.labrep,case_label=label)
dev.off()

#ERROR
source("LengthsLL4.r") 
pdf("figs\\Fig 9 LengthsLL4.pdf",width=5, height=11.5)
LengthsLL4(labrep.file=example.labrep,case_label=label)
dev.off()

###Fig 10 same page plot comparing fits to CPUE and aerial survey
source("Compare fits.r")
pdf(file="figs\\Fig 10 SurveyIndexFits.pdf",width=4,height=8)
example.obj <- data.base["base2010sqrt_h2m3M3O2C2a1_lab.rep"]
compare.fits(compare.objects=example.obj,lab.cex=1.1,
             case_label=label, legend=names(example.obj))
#compare.fits(compare.objects=data.base[1:5],lab.cex=1.1,
#             case_label="example", legend=names(data.base[1:5]))
dev.off()

####Fig 11 Combine tagger groups and look at fits to tagging data
source("Tagging fits combine tagger groups.r") 
pdf("figs\\Fig 11 Tagfits combine taggers.pdf",width=6,height=8)
tagging.fits(data.object = example.obj,case_label=label)
dev.off()

####Fig 12 Tagging fits on pooled tagging data
source("Tagging fits pooled.r")
pdf("figs\\Fig 12 Tagfits pooled.pdf",width=3,height=8)
###Does not work on sbtmod21 (since the tagging likelihood there is on pooled taggging data)
tagging.fits.pooled(data.object = example.obj, ages=2:8, years=1992:1997,case_label=label)
dev.off()

####Fig 13 Tagging fits same year
source("Tagging fits same year.r")
pdf("figs\\Fig 13 Tagfits same year.pdf",width=3,height=8)
tagging.fits.same.age(data.object = example.obj, ages=1:8, years=1991:1997,case_label=label)
dev.off()

####Fig 14 Tagging fits subsequent years
source("Tagging fits subsequent.r")
pdf("figs\\Fig 14 Tagfits subsequent.pdf",width=12,height=5)
tagging.fits(data.object = example.obj,case_label=label)
dev.off()

####Fig 15 Recruitment with SE from std file
#NEEDS .std file to be produced
#ERROR
source("Recruitment.r")
pdf(file="figs\\Fig 15 Recruitment.pdf",width=8,height=8)
x <- plot.recruitment(std.file="std files\\sbtmod24.std", labrep.file="std files\\sbtmod24_lab.rep",case_label="example midc1s1l1")
dev.off()


####Fig 16 SD residuals
source("SD residuals.r")
pdf("figs\\Fig 16 SD resids.pdf",width=6,height=8)
hist.SD.resids(data.objects=data.base, label="base2010sqrt")
dev.off()

####Fig 17 Fits to survey indices (not comparing across model runs, just one model run
source("SurveyIndexFits.r")
pdf(file="figs\\Fig 17 SurveyIndexFits.pdf",width=4,height=8)
fit.all(data.object=example.obj,lab.cex=1.1,case_label=label)
dev.off()

####Fig 18 Plus group check with Indonesian selectivity and M
source("PlusGroup9by3.r")
pdf("figs\\Fig 18 Plus Group9by3 base basenoCK.pdf",width=11.5,height=6)

rand.set <- sample(x=1:320,size=9,replace=F)
compare.plus.groups2(compare.objects=data.base[rand.set],
                     ylim=c(0,1), label="base2010sqrt")
compare.plus.groups2(compare.objects=data.base.noCK[rand.set],
                     ylim=c(0,1), label="basenoCK")
#rand.set <- sample(x=1:256,size=9,replace=F)
#compare.plus.groups2(compare.objects=data.baseCKmk5[rand.set],
#                     ylim=c(0,1), label="baseCKmk5 different randoms")
dev.off()

#####Table 1 Recruitment autocorrel and penalties
#by pairs of decades (30s-40s, 50s-60s, ... 90s-00s)
source("Rec autocorrel pen.r")
Rec.autocorrel.pen(data.objects=data.base,label="")

####Fig 19 compare likelihood components by decade for the 
#two-decade groupings of the recruitment penalties
source("TableLikeRecDecades.r")
nll.recdec.base  <- likelihood.recdec(data.objects = data.base)
nll.recdec.base.NS  <- likelihood.recdec(data.objects = data.base.NS)

source("PlotNLLRecDecades.r")
pdf("figs\\Fig 19 NLLs recdec base.pdf", height=9,width=8) 
plot.NLL.recdec(nll.recdec.base,caption="base2010sqrt")
dev.off()

pdf("figs\\Fig 19b NLLs recdec newsel.pdf", height=9,width=8) 
plot.NLL.recdec(nll.recdec.base.NS,caption="base2010sqrt new sel")
dev.off()

source("Rec pen and dev by yr.r")
temp <- plot.rec.dev(data.objects=data.base)
pdf("figs\\Fig 20 rec and devs by yr.pdf", height=9,width=8) 
par(mfrow=c(2,1), oma=c(3,3,1,1), mar=c(0,0,2,0))
plot(x=1931:2012, temp[1,], xaxt="n", pch=19,main="Recruitment deviates")
plot(x=1931:2012, temp[1,]^2/(2*0.6^2), pch=19, main="Recruitment NLL penalty")
dev.off()

########======Fig 20b Recs with new Sel
#data.midcellNewSel <- get.all.files("arc\\midcellNewSel")
#data.midcellsqrt <- get.all.files("arc\\midcellsqrt")

pdf("figs\\Fig 20b rec and devs NEW SEL.pdf", height=7,width=6) 
par(mfrow=c(2,1), oma=c(3,3,1,1), mar=c(0,0,2,0))
plot(x=1931:2012, y=data.midcellNewSel[[1]]$Rdev, ylim=c(-1.3,1), xaxt="n", 
     col="#FF000044", pch=19,main="Deviates by Sel new=red old=blue")
par(new=T)
plot(x=1931:2012, y=data.midcellsqrt[[1]]$Rdev, ylim=c(-1.3,1),  xaxt="n", 
     col="#0000FF44", pch=19, main="")

plot(x=1931:2012, y=data.midcellNewSel[[1]]$Rdev^2/(2*0.6^2), ylim=c(0,2), xaxt="n", 
     col="#FF000044", pch=19,main="NLL penalty new=red old=blue")
par(new=T)
plot(x=1931:2012, y=data.midcellsqrt[[1]]$Rdev^2/(2*0.6^2), ylim=c(0,2),  xaxt="n", 
     col="#0000FF44", pch=19, main="")
axis(1)
dev.off()


#======================Fig 21
pdf("figs\\Fig 21 boxplots rec and devs by yr.pdf", height=9,width=8) 
par(mfrow=c(2,1), oma=c(3,3,1,1), mar=c(0,0,5,0))
boxplot(temp,names=1931:2012, main="Recruitment deviates")
boxplot(temp^2/(2*0.6^2), names=1931:2012, main="Recruitment NLL penalty")
dev.off()

#***Fig 22 Shaded plot ns, emature
#make a 150KB file instead of 1.5 MB file
source("ShadedPlots.r")
jpeg(file="figs\\Fig 22 Shaded newsel.jpg", width=600,height=600)
plot.lev("levfiles\\base2010sqrtns.lev")
dev.off()
jpeg(file="figs\\Fig 22 Shaded newsel early mature.jpg", width=600,height=600)
plot.lev("levfiles\\base2010sqrtns_emature.lev")
dev.off()
jpeg(file="figs\\Fig 22 Shaded newsel lnq.jpg", width=600,height=600)
plot.lev("levfiles\\base2010sqrtns_lnq.lev")
dev.off()

####Fig 23 Plus group check with autocorrelation and early mature
source("PlusGroup9by3.r")
pdf("figs\\Fig 23 Plus Group9by3 early selectivity.pdf",width=11.5,height=6)

rand.set <- sample(x=1:320,size=9,replace=F)
compare.plus.groups2(compare.objects=data.base.NS[rand.set],
                     ylim=c(0,1), label="new sel")
compare.plus.groups2(compare.objects=data.base.NS_emature[rand.set],
                     ylim=c(0,1), label="new sel early mature")
compare.plus.groups2(compare.objects=data.base.NS_lnq[rand.set],
                     ylim=c(0,1), label="new sel lnq")
dev.off()


####Fig 24 Recruitment
pdf("figs\\Fig 24 Recruitment new sel compare.pdf",width=8,height=6)
source("Compare recruitment.r")
rand.set <- sample(x=1:320,size=20,replace=F)
for (i in 1:length(rand.set)) {
   compare.recruitment(compare.objects=list(data.base.NS[[rand.set[i]]],
                                            data.base.NS_emature[[rand.set[i]]]),
                    lab.cex=1,R.ylim=NULL,Rdev.ylim=NULL,
                    label=data.base.NS[[rand.set[i]]]$scenario_number,
                    legend=c("New sel","Early mature")) 
}
dev.off()

####Fig 25 Total biomass 10+
#Note: this is the variability in the 320 cells, not the 2000 grid
pdf("figs\\Fig 25 Total biomass 10+ new sel.pdf",width=6,height=4)
source("SSB10plus boxplots.r")
temp1 <- plot.SSB10plus.boxplots.r(objects=data.base.NS, label="New selectivity")
temp2 <- plot.SSB10plus.boxplots.r(objects=data.base.NS_emature, label="Early mature")
dev.off()
rbind(temp1[2,], temp2[2,])

####Fig 26 compare likelihood components by decade for the 
#two-decade groupings of the recruitment penalties
source("TableLikeRecDecades.r")
nll.recdec.base.NS  <- likelihood.recdec(data.objects = data.base.NS)

source("PlotNLLRecDecades.r")
pdf("figs\\Fig 26 NLLs new sel.pdf", height=9,width=8) 
plot.NLL.recdec(nll.recdec.base.NS,caption="New selectivity")
dev.off()

####Fig 27 SSB quantiles
#Note: this is the variability in the 320 cells, not the 2000 grid
pdf("figs\\Fig 27 Quantiles SSB new sel.pdf",width=6,height=4)
source("SSB quantiles.r")
temp3 <- plot.SSB.quantplots.r(objects=data.base.NS, label="New selectivity")
temp4 <- plot.SSB.quantplots.r(objects=data.base.NS_emature, label="Early mature")
dev.off()
rbind(temp3[2,], temp4[2,])

####Fig 28 SSB and R quantiles
#Note: this is the variability in the 320 cells, not the 2000 grid
pdf("figs\\Fig 28 Quantiles SSB and R.pdf",width=6,height=11)
source("SSB and r quantiles.r")
temp3 <- plot.SSB.r.quantplots(objects=data.base.NS, label="New selectivity")
temp4 <- plot.SSB.r.quantplots(objects=data.base.NS_emature, label="Early mature")
dev.off()
#rbind(temp3[2,], temp4[2,])

####Fig 29 median SSB vs R quantiles
#Note: this is the variability in the 320 cells, not the 2000 grid
pdf("figs\\Fig 29 SSB vs R.pdf",width=6,height=11)
plot(x=temp3[[1]][2,], y=temp3[[2]][2,], xlim=c(0,1), ylim=c(0,2.5))
plot(x=temp4[[1]][2,], y=temp4[[2]][2,], xlim=c(0,1), ylim=c(0,2.5))
dev.off()

####Fig 30 compare likelihood components 
#for components of the base.NS and base.NS.AC
#the complication being that some of the cells of the AC one 
#failed and hence must be stripped from both

#Find the cells where the objective function is "nan"
ncells <- length(data.base.AC)
objF <- vector(length=ncells)
for (i in 1:ncells) {
   objF[i] <- data.base.AC[[i]]$ObjF  
}
objF

#Find the cells where the maximum gradient is greater than a certain value
last.phase <- read.csv("arc\\base2010sqrtAC\\base2010sqrt.last_phase",
                       sep=" ", header=F)
max.grad <- last.phase[,18]
par(mfrow=c(1,1))
col <- c(rep(1,64), rep(2,64), rep(3,64), rep(4,64), rep(5,64))
plot(max.grad,log="y", col=col)

#find the cells that did NOT fail: definition that objF<>NA
success.cells <- (1:320)[(objF != "nan") & (max.grad < 1)]

source("TableLikeRecDecades.r")
nll.recdec.base.NS  <- likelihood.recdec(data.objects = data.base.NS[success.cells])
nll.recdec.base.AC  <- likelihood.recdec(data.objects = data.base.AC[success.cells])

source("PlotNLLRecDecades.r")
pdf("figs\\Fig 30 NLLs new sel.pdf", height=9,width=8) 
plot.NLL.recdec(nll.recdec.base.NS,caption="New selectivity")
plot.NLL.recdec(nll.recdec.base.AC,caption="Autocorrelation")
dev.off()


####Fig 31 compare likelihood components  base7h
#for components of the base.NS and base.NS.AC
#the complication being that some of the cells of the AC one 
#failed and hence must be stripped from both

#Find the cells where the objective function is "nan"
ncells <- length(data.base7h)
objF <- vector(length=ncells)
for (i in 1:ncells) {
   objF[i] <- data.base7h[[i]]$ObjF  
}
objF

#Find the cells where the maximum gradient is greater than a certain value
last.phase <- read.csv("arc\\base7hsqrt\\base7hsqrt.last_phase",
                       sep=" ", header=F)
max.grad <- last.phase[,18]
par(mfrow=c(1,1))
col <- c(rep(1,64), rep(2,64), rep(3,64), rep(4,64), rep(5,64))
plot(max.grad,log="y", col=col)

#find the cells that did NOT fail: definition that objF<>NA
success.cells <- (1:448)[(objF != "nan") & (max.grad < 1)]

source("TableLikeRecDecades.Ana.r")
nll.recdec.base7h  <- likelihood.recdec(data.objects = data.base7h[success.cells])

source("PlotNLLRecDecades.r")
pdf("figs\\Fig 31 NLLs base7h.pdf", height=9,width=8) 
plot.NLL.recdec(nll.recdec.base7h,caption="Base7h")
dev.off()

####Fig 32 Total biomass 10+ base7h
#Note: this is the variability in the 320 cells, not the 2000 grid
pdf("figs\\Fig 32 Total biomass 10+ base7h.pdf",width=6,height=4)
source("SSB10plus boxplots.r")
temp1 <- plot.SSB10plus.boxplots.r(objects=data.base7h[success.cells], label="Base7h")
temp2 <- plot.SSB10plus.boxplots.r(objects=data.base.NS, label="Base")
dev.off()

####Fig 33 SSB and R quantiles
#Note: this is the variability in the 320 cells, not the 2000 grid
pdf("figs\\Fig 33 Quantiles SSB and R base7h.pdf",width=6,height=11)
source("SSB and r quantiles.r")
temp3 <- plot.SSB.r.quantplots(objects=data.base7h[success.cells], label="Base7h")
temp4 <- plot.SSB.r.quantplots(objects=data.base.NS, label="Base")
dev.off()
#rbind(temp3[2,], temp4[2,])

####Fig 34 Total biomass 10+ base7h
#Note: this is the variability in the 320 cells, not the 2000 grid
pdf("figs\\Fig 34 Total biomass 10+ lambdas.pdf",width=6,height=4)
source("SSB10plus boxplots.r")
temp11 <- plot.SSB10plus.boxplots.r(objects=data.c1lambda1, label="c1lambda1")
temp21 <- plot.SSB10plus.boxplots.r(objects=data.c2lambda1, label="c2lambda1")
temp31 <- plot.SSB10plus.boxplots.r(objects=data.c3lambda1, label="c3lambda1")
temp41 <- plot.SSB10plus.boxplots.r(objects=data.c4lambda1, label="c4lambda1")
temp425 <- plot.SSB10plus.boxplots.r(objects=data.c4lambda25, label="c4lambda25")
temp450 <- plot.SSB10plus.boxplots.r(objects=data.c4lambda50, label="c4lambda50")
temp475 <- plot.SSB10plus.boxplots.r(objects=data.c4lambda75, label="c4lambda75")
dev.off()

####Fig 35 SSB and R quantiles
#Note: this is the variability in the 320 cells, not the 2000 grid
pdf("figs\\Fig 35 Quantiles SSB and R lambdas.pdf",width=6,height=11)
source("SSB and r quantiles.r")
temp11 <- plot.SSB.r.quantplots(objects=data.c1lambda1, label="c1lambda1")
temp21 <- plot.SSB.r.quantplots(objects=data.c2lambda1, label="c2lambda1")
temp31 <- plot.SSB.r.quantplots(objects=data.c3lambda1, label="c3lambda1")
temp41 <- plot.SSB.r.quantplots(objects=data.c4lambda1, label="c4lambda1")
temp425 <- plot.SSB.r.quantplots(objects=data.c4lambda25, label="c4lambda25")
temp450 <- plot.SSB.r.quantplots(objects=data.c4lambda50, label="c4lambda50")
temp475 <- plot.SSB.r.quantplots(objects=data.c4lambda75, label="c4lambda75")
dev.off()
#rbind(temp3[2,], temp4[2,])

c(temp11$SSBquants[2,83],temp21$SSBquants[2,83],temp31$SSBquants[2,83],
  temp41$SSBquants[2,83],temp425$SSBquants[2,83],
  temp450$SSBquants[2,83],temp475$SSBquants[2,83])
temp11$SSBquants[2,83]
temp11

####Fig 36 SSB, B10+ and R comparing these for the 7 lambda schedules
pdf("figs\\Fig 36 B10+ SSB R compare lambdas.pdf",width=5,height=8)
source("plot.B10.SSB.R.compare.r")
lambda.obj <- list(data.c1lambda1[[1]],data.c2lambda1[[1]],
                   data.c3lambda1[[1]],data.c4lambda1[[1]],
                   data.c4lambda25[[1]],data.c4lambda50[[1]],
                   data.c4lambda75[[1]])
plot.B10.SSB.R.compare(objects=lambda.obj, abs.rel="rel",
         label="Maturity schedules",
         legend=c("c1lambda1","c1lambda1","c3lambda1","c4lambda1",
                  "c4lambda25", "c4lambda50", "c4lambda75"))
plot.B10.SSB.R.compare(objects=lambda.obj, abs.rel="abs",
                       label="Maturity schedules",
                       legend=c("c1lambda1","c1lambda1","c3lambda1","c4lambda1",
                                "c4lambda25", "c4lambda50", "c4lambda75"))
dev.off()

#***Fig 37 Shaded plot base
#make a 150KB file instead of 1.5 MB file
source("ShadedPlots.r")
jpeg(file="figs\\Fig 37 Shaded final base unif h prior.jpg", width=600,height=600)
plot.lev("levfiles\\base2010sqrtns.lev")
dev.off()

#***Fig 39 MSY on the final base
#Plot the MSY, Fmsy, Bmsy (final year, can also do in every year)
#source("get.all.files.r")
#data.base <- get.all.files("arc\\base2010sqrt")
source("MSY values.r")
pdf(file="figs\\Fig 39 MSY.pdf",width=10,height=8)
x <- MSY.vals(data.objects=data,label="data.base.NS")
dev.off()


#====Fig 40 NLL plots on base6Msqrt
source("TableLikeRecDecades.r")
nll.recdec.base6M  <- likelihood.recdec(data.objects = data.base6M, AC=0)

source("PlotNLLRecDecades.r")
pdf("figs\\Fig 40 NLLs base6M.pdf", height=9,width=8) 
plot.NLL.recdec(nll.recdec.base6M,caption="Base6M with AC=0.0")
dev.off()

#====Fig 41 NLL plots on baseAC12
source("TableLikeRecDecades.r")
nll.recdec.baseAC12  <- likelihood.recdec(data.objects = data.base.AC12, AC=0.5)

source("PlotNLLRecDecades.r")
pdf("figs\\Fig 41 NLLs baseAC12.pdf", height=9,width=8) 
plot.NLL.recdec(nll.recdec.baseAC12,caption="BaseAC12 with AC=0.5")  
dev.off()

#=========Fig 42-43 shaded lev plots
source("ShadedPlots.r")
jpeg(file="figs\\Fig 42 Shaded base6M.jpg", width=600,height=600)
plot.lev("levfiles\\base6Msqrt.lev")
dev.off()

jpeg(file="figs\\Fig 43 Shaded baseAC12.jpg", width=600,height=600)
plot.lev("levfiles\\base2012sqrtAC.lev")
dev.off()
