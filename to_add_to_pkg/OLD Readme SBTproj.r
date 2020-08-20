######################################################################
#Example R code for running code on future projections and the full grid
#main22 c1s1l1 sqrt
#main22 c1s1l1 orig.5
#sample_v4 c1s1l1 c1s1l1 sqrt
#sbtprojv117 -ctrfile mycontrol_111.dat
#
#this will produce a series of files ending in .s1, .s2 etc.
#
#R code here programmed by Trevor A Branch in June and July 2009 tbranch@gmail.com
#Help from Jim Ianelli and Ana Parma at various points. 
#Feel free to use and reuse at will; if included in a scientific publication, an acknowledgement 
#would be appreciated. 
#Dates: 10-25 July 2009
#######################################################################
library(PBSmodelling)   #for reading in the lab.rep files
library(gplots)         #for the rich.colors() function by Arni Magnusson

######################################################################
#Jim's scatterplot, showing the sampling of cells in a grid (uses .lev file)
######################################################################
source("ShadedPlots.r")
jpeg(file="figs\\ShadedPlot.jpg", width=600,height=600)
plot.lev("C1S1L1.lev",title="Example shaded plot")
dev.off()

######################################################################
#Projections of spawning biomass and recruitment
######################################################################
source("SRquantiles.r")
pdf("figs//SRexample.pdf",height=10,width=10,pointsize=12)
s4filename <- c("projections\\m4bounded\\C1S1L1m4bounded.s4")
caption <- c("M(4) is bounded, 11810 mt")
SRquantiles(s4filename=s4filename,caption=caption, R.ylim=c(0,30), B.ylim=c(0,1800), xlim = c(1930.5,2032.5))
dev.off()

######################################################################
#Plots of stock and recruitment, comparing the values for different 
#levels of parameters, thus far implemented for:
#capt.levels = "m"     natural mortality at age 1
#capt.levels = "M10"   natural mortality at age 10
#capt.levels = "h"     steepness
#capt.levels = "O" [the capital letter O], omega, the relationship between CPUE and abundance
#capt.levels = "C"     CPUE level
#capt.levels = "a"     age range for CPUE
#capt.levels = "sample"  weighting on samples (sqrt vs orig.5)
#Note that the function is hard-wired to look at the second item in the .s4 filename
#which it assumes will be a seven digit number: h, m, M10, O, CPUE, q_age_range, sample
#changing the output will alter the function.
######################################################################

s4filename <- c("projections\\m4bounded\\C1S1L1m4bounded.s4")  #SAME s4 FILENAME FOR ALL OPTIONS

source("SRquantilesLevels.r")
pdf("figs//M4bounded_steeplevels.pdf",height=10,width=10,pointsize=12)
caption <- c("M4 bounded 11810 mt")
SRquantilesLevels(s4filename=s4filename,caption=caption, R.ylim=c(0,20), B.ylim=c(0,1950), xlim = c(1930.5,2032.5),capt.levels="h")
dev.off()

source("SRquantilesLevels.r")
pdf("figs//M4bounded_mlevels.pdf",height=10,width=10,pointsize=12)
caption <- c("M4 bounded 11810 mt")
SRquantilesLevels(s4filename=s4filename,caption=caption, R.ylim=c(0,30), B.ylim=c(0,1800), xlim = c(1930.5,2032.5),capt.levels="m")
dev.off()

source("SRquantilesLevels.r")
pdf("figs//M4bounded_M10levels.pdf",height=10,width=10,pointsize=12)
caption <- c("M4 bounded 11810 mt")
SRquantilesLevels(s4filename=s4filename,caption=caption, R.ylim=c(0,30), B.ylim=c(0,1800), xlim = c(1930.5,2032.5),capt.levels="M10")
dev.off()

source("SRquantilesLevels.r")
pdf("figs//M4 bounded_Omegalevels.pdf",height=10,width=10,pointsize=12)
caption <- c("M4 bounded 11810 mt")
SRquantilesLevels(s4filename=s4filename,caption=caption, R.ylim=c(0,18), B.ylim=c(0,1950), xlim = c(1930.5,2032.5),capt.levels="O")
dev.off()

source("SRquantilesLevels.r")
pdf("figs//M4bounded_CPUElevels.pdf",height=10,width=10,pointsize=12)
caption <- c("M4 bounded 11810 mt 1=sqrt 2=orig.5")
SRquantilesLevels(s4filename=s4filename,caption=caption, R.ylim=c(0,18), B.ylim=c(0,1950), xlim = c(1930.5,2032.5),capt.levels="CPUE")
dev.off()

source("SRquantilesLevels.r")
pdf("figs//M4bounded_AgeRangelevels.pdf",height=10,width=10,pointsize=12)
caption <- c("M4 bounded 11810 mt 1=sqrt 2=orig.5")
SRquantilesLevels(s4filename=s4filename,caption=caption, R.ylim=c(0,18), B.ylim=c(0,1950), xlim = c(1930.5,2032.5),capt.levels="AgeRange")
dev.off()

#IMPORTANT: the plot depends on the order in which sample_v4 was called. The first plot will be whichever was first. 
#e.g. if sample_v4 c1s1l1 sqrt orig.5 then first plot sqrt second plot orig.5
source("SRquantilesLevels.r")
pdf("figs//M4bounded_samplelevels.pdf",height=10,width=10,pointsize=12)
caption <- c("M4 bounded 11810 mt 1=sqrt 2=orig.5")
SRquantilesLevels(s4filename=s4filename,caption=caption, R.ylim=c(0,18), B.ylim=c(0,1950), xlim = c(1930.5,2032.5),capt.levels="sample")
dev.off()