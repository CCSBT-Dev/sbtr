#===============================================================
#List of r code used in Seattle WA meeting of CCSBT
#24-27 June 2014.
#Written by Trevor A. Branch tbranch@gmail.com
#University of Washington
#===============================================================

#=====Required packages==============
#install.packages("PBSmodelling")
require(PBSmodelling)

####################################################################
#Read in all data from the .lab rep files. 
source("get.all.files.r")
#This takes a long time (10-15 minutes)
#data.IS20 <- get.all.files("arc\\base2013_IS20sqrt_IS20")
#data.base <- get.all.files("arc\\base2013sqrt_UA")
#data.midcell <- get.all.files("arc\\midcellsqrt")
#data.odtag <- get.all.files("arc\\base2013sqrt_odtag")      #likelihood profiles
#data.IndoSel <- get.all.files("arc\\base2013sqrt_IndoSel")  #indo plots

example.labrep <- "arc\\base2013sqrt_UA\\base2013sqrt_UA_h2m3M3O2C2a1_lab.rep"
example.obj <- data.base[["base2013sqrt_UA_h2m3M3O2C2a1_lab.rep"]]
label <- "h2m3M3O2C2a1"

########Fig. 1 likelihood plot comparisons by steepness
source("TableLikelihoodComponents v2.r")
nll.table.base  <- likelihood.table2(data.objects = data.base)
nll.table.IS20  <- likelihood.table2(data.objects = data.IS20)
write.csv(file="tables\\baseNLL.csv",nll.table.base)
write.csv(file="tables\\IS20.csv",nll.table.IS20)


source("PlotNLLComponents.r")
pdf("figs\\Fig 1 NLLs base vs IS20.pdf") 
plot.NLL.by.steepness(nll.table.base,caption="base")
plot.NLL.by.steepness(nll.table.IS20,caption="IS20")
dev.off()

########Fig. 2 Plus group check with Indonesian selectivity and M
source("PlusGroup9by3.r")
pdf("figs\\Fig 2 Plus Group base vs IS20 temp.pdf",width=11.5,height=6)
ndata <- length(data.base)
rand.set <- sample(x=1:ndata,size=9,replace=F)
compare.plus.groups2(compare.objects=data.base[rand.set],
                     ylim=c(0,1), label="base")
ndata <- length(data.IS20)
rand.set <- sample(x=1:ndata,size=9,replace=F)
compare.plus.groups2(compare.objects=data.IS20[rand.set],
                     ylim=c(0,1), label="IS20")
dev.off()

########Fig. 3 likelihood plot comparisons by M10
source("TableLikelihoodComponents v2.r")
nll.table.base  <- likelihood.table2(data.objects = data.base)
nll.table.IS20  <- likelihood.table2(data.objects = data.IS20)

source("PlotNLLbyM.r")
pdf("figs\\Fig 3 NLLs by M10 base vs IS20.pdf") 
plot.NLL.by.M(nll.table.base,caption="base")
plot.NLL.by.M(nll.table.IS20,caption="IS20")
dev.off()

########Fig. 4 MSY plots in final year
#Plot the MSY, Fmsy, Bmsy (final year, can also do in every year)
source("MSY values.r")
pdf(file="figs\\Fig 4 MSY base.pdf",width=10,height=8)
x <- MSY.vals(data.objects=data.base,label="")
dev.off()

########Fig. 5 Shaded plots
#make a 150KB file instead of 1.5 MB file
source("ShadedPlots.r")
jpeg(file="figs\\Fig 5 Shaded base.jpg", width=600,height=600)
plot.lev("levfiles\\base2013sqrt_UA.lev")
dev.off()
jpeg(file="figs\\Fig 5 Shaded IS20.jpg", width=600,height=600)
plot.lev("levfiles\\base2013_IS20sqrt_IS20.lev")
dev.off()

########Fig. 6 Aussie age fits
source("AussieAgeFits.r")
pdf("figs\\Fig 6 AussieAge.pdf",width=6, height=11.5)
AussieAgeFits(labrep.file=example.labrep,case_label="base mid cell: h1m1M2O2C3a2")
dev.off()  #to close pdf file, when errors occur type this in a few times. 


######Fig 7-10 Length fits to LL1, LL2, LL3, LL4
source("LengthsLL1.r")
pdf("figs\\Fig 7 LengthsLL1.pdf",width=8, height=11.5)
LengthsLL1(labrep.file=example.labrep,case_label=label)
dev.off()

source("LengthsLL2.r")
pdf("figs\\Fig 8 LengthsLL2.pdf",width=6, height=8)
LengthsLL2(labrep.file=example.labrep,case_label=label)
dev.off()

source("LengthsLL3.r")
pdf("figs\\Fig 9 LengthsLL3.pdf",width=8, height=11.5)
LengthsLL3(labrep.file=example.labrep,case_label=label)
dev.off()

source("LengthsLL4.r") 
pdf("figs\\Fig 10 LengthsLL4.pdf",width=5, height=11.5)
LengthsLL4(labrep.file=example.labrep,case_label=label)
dev.off()

#######Fig 11 fits to CPUE and aerial survey
#example.obj <- data.base[["base2013sqrt_UA_h2m3M3O2C2a1_lab.rep"]]
source("Compare fits.r")
pdf(file="figs\\Fig 11 SurveyIndexFits.pdf",width=4,height=8)
compare.fits(compare.objects=list(example.obj),lab.cex=1.1,
             case_label=label, legend=names(example.obj))
dev.off()


####Fig 12 Combine tagger groups and look at fits to tagging data
source("Tagging fits combine tagger groups.r") 
pdf("figs\\Fig 12 Tagfits combine taggers.pdf",width=6,height=8)
tagging.fits(data.object = example.obj,case_label=label)
dev.off()

####Fig 13 Tagging fits on pooled tagging data
source("Tagging fits pooled.r")
pdf("figs\\Fig 13 Tagfits pooled.pdf",width=3,height=8)
###Does not work on sbtmod21 (since the tagging likelihood there is on pooled taggging data)
tagging.fits.pooled(data.object = example.obj, ages=2:8, years=1992:1997,case_label=label)
dev.off()

####Fig 14 Tagging fits same year
source("Tagging fits same year.r")
pdf("figs\\Fig 14 Tagfits same year.pdf",width=3,height=8)
tagging.fits.same.age(data.object = example.obj, ages=1:8, years=1991:1997,case_label=label)
dev.off()

####Fig 15 Tagging fits subsequent years
source("Tagging fits subsequent.r")
pdf("figs\\Fig 15 Tagfits subsequent.pdf",width=12,height=5)
tagging.fits(data.object = example.obj,case_label=label)
dev.off()

####Fig 16 Fits to survey indices (one model run)
source("SurveyIndexFits.r")
pdf(file="figs\\Fig 16 SurveyIndexFits.pdf",width=4,height=8)
fit.all(data.object=example.obj,lab.cex=1.1,case_label=label)
dev.off()

####Fig 17 Fits to Indonesian age data
source("IndoAgeFits.r")
IS20.obj <- data.IS20[["base2013_IS20sqrt_IS20_h2m3M3O2C2a1_lab.rep"]]
pdf(file="figs\\Fig 17 IndonesianAge IS20.pdf",width=6, height=11.5)
IndoAgeFits(data.object=IS20.obj,case_label="IS20 mid cell")
dev.off()


####Fig 18 Fits to Indonesian age data IS20
source("IndoAgeFits.r")
pdf("figs\\Fig 18 IndonesianAge.pdf",width=6, height=11.5)
IndoAgeFits(data.object=example.obj,case_label=label)
dev.off()

example.obj$age.obs.1

####Fig 19 Residuals between obs and pred CPUE by length for LL1
#require(PBSmodelling)
#source("get.all.files.r")
#data.CPUE <- get.all.files("arc\\CPUEbyLength")
source("CPUE by length.r")
pdf(file="figs\\Fig 19 CPUEresid.pdf", width=8,height=11.5)
par(mfrow=c(2,1), oma=c(0,0,0,0), mar=c(5,5,3,1)) 
CPUE.by.length(data.objects = data.CPUE, pt.scaling=150)
dev.off()


####Fig 20 Selectivity plots when the Indo 
#selectivity is changed every two years but in 2012 *and* 2013
source("PlusGroup9by3.r")
pdf("figs\\Fig 20 Indo sel changed in 2013.pdf",width=11.5,height=6)

ndata <- length(data.base)
rand.set <- sample(x=1:ndata,size=9,replace=F)
compare.plus.groups2(compare.objects=data.base[rand.set],
                     ylim=c(0,1), label="2013 base case",
                     sel.years=c(1952, 1994, 1998, 2002, 2004,2006,2008,2010,2012,2013))

ndata <- length(data.IndoSel)
#rand.set <- sample(x=1:ndata,size=9,replace=F)
compare.plus.groups2(compare.objects=data.IndoSel[rand.set],
                     ylim=c(0,1), label="2013 new Indo selectivity",
                     sel.years=c(1952, 1994, 1998, 2002, 2004,2006,2008,2010,2012,2013))

ndata <- length(data.odtag)
#comment out the next line to get the exact same scenarios
#rand.set <- sample(x=1:ndata,size=9,replace=F)
compare.plus.groups2(compare.objects=data.odtag[rand.set],
                     ylim=c(0,1), label="Over-dispersion tagging",
                     sel.years=c(1952, 1994, 1998, 2002, 2004,2006,2008,2010,2012,2013))
dev.off()

####Fig 21 Fits to Indonesian age data IS20
source("IndoAgeFits.r")
Indo.obj <- data.IndoSel[["base2013sqrt_IndoSel_h2m3M3O2C2a1_lab.rep"]]
Base.obj <- data.base[["base2013sqrt_UA_h2m3M3O2C2a1_lab.rep"]]
pdf("figs\\Fig 21 IndonesianAge.pdf",width=6, height=11.5)
IndoAgeFits(data.object=Indo.obj,case_label="2013 Indo selectivity")
IndoAgeFits(data.object=Base.obj,case_label="Base")
dev.off()
