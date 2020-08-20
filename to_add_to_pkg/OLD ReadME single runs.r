############################################################################
#Complete set of fits of the model sbtmodxx.tpl to the data for southern bluefin tuna.
#Required the following files: something ending in "_lab.rep", assuming that the naming convention in the file has sections of 
#outputs separated by $name comments so that it can be easily parsed by readList() function of Jon Schnute 
#library PBSmodelling required, available at http://code.google.com/p/pbs-software/downloads/list
#then in R go to menu Packages, Install Package from Local zip file
#Best results obtained when outputting to pdf files
#Run by:
#Programmed by Trevor A. Branch, tbranch@gmail.com, June-July 2009
###run using source("ViewerJuly09.r")
##########################################################################
library(PBSmodelling)  #for readList
library(gplots)        #for rich.colors()

labrep22.file <- "sbtmod22_lab.rep"
labrep21.file <- "sbtmod21_lab.rep"
std.file <- "sbtmod22.std"  #only needed for the recruitment plots
output.dir <- "figs"
case_label <- "c1s1l1orig.5_h1m1M1O1C2a1"

####Fits of the model to the CPUE, 
pdf(paste(output.dir,"\\","SurveyIndexFits.pdf",sep=""),width=4,height=8)
source("SurveyIndexFits.r")
fit.all(labrep.file=labrep22.file,lab.cex=1.1,case_label=case_label)
dev.off()

####Recruitment estimates over time, requires an .std file to get the standard errors
source("Recruitment.r")
pdf(paste(output.dir,"\\","Recruitment.pdf",sep=""),width=8,height=8)
x <- plot.recruitment(std.file=std.file, labrep.file=labrep22.file,case_label=case_label)
dev.off()

####Tagging fits pooled for both sbtmod21 and sbtmod22
source("Tagging fits pooled.r")
pdf(file=paste(output.dir,"\\","Tagfits pooled.pdf",sep=""),width=3,height=8)
tagging.fits.pooled(labrep.file = labrep22.file, ages=2:8, years=1992:1997, case_label=case_label)
dev.off()

####Tagging fits for sbtmod22 only, for recaptures in years after marking, colors are redder with more data
source("Tagging fits subsequent.r")
pdf(paste(output.dir,"\\","Tagfits subsequent.pdf",sep=""),width=12,height=5)
tagging.fits(labrep.file = labrep22.file, case_label=case_label)
dev.off()

####Tagging fits for sbtmod22 only, for recaptures in same year as marking
source("Tagging fits same year.r")
pdf(paste(output.dir,"\\","Tagfits same year.pdf",sep=""),width=3,height=8)
tagging.fits.same.age(labrep.file = labrep22.file, ages=1:8, years=1991:1997, case_label=case_label)
dev.off()

####Fits of the model to the Australian ages
source("AussieAgeFits.r")
pdf(paste(output.dir,"\\","AussieAge.pdf",sep=""),width=6, height=11.5)
AussieAgeFits(labrep.file=labrep22.file,case_label=case_label)
dev.off()

####Fits of the model to the Indonesian ages
source("IndonesianAgeFits.r")
pdf(paste(output.dir,"\\","IndonesianAge.pdf",sep=""),width=6, height=11.5)
IndonesianAgeFits(labrep.file=labrep22.file,case_label=case_label)
dev.off()

####Fits of the model to length frequencies from LL1
source("LengthsLL1.r")
pdf(paste(output.dir,"\\","LengthsLL1.pdf",sep=""),width=8, height=11.5)
LengthsLL1(labrep.file=labrep22.file,case_label=case_label)
dev.off()

####Fits of the model to length frequencies from LL2
source("LengthsLL2.r")
pdf(paste(output.dir,"\\","LengthsLL2.pdf",sep=""),width=8, height=11.5)
LengthsLL2(labrep.file=labrep22.file,case_label=case_label)
dev.off()

####Fits of the model to length frequencies from LL3
source("LengthsLL3.r")
pdf(paste(output.dir,"\\","LengthsLL3.pdf",sep=""),width=8, height=11.5)
LengthsLL3(labrep.file=labrep22.file,case_label=case_label)
dev.off()

####Fits of the model to length frequencies from LL4
source("LengthsLL4.r")
pdf(paste(output.dir,"\\","LengthsLL4.pdf",sep=""),width=5, height=11.5)
LengthsLL4(labrep.file=labrep22.file,case_label=case_label)
dev.off()
