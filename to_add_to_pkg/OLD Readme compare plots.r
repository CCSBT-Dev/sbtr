######################################################################
#Example R code for running model fits on various _lab.rep files
#produced by running the main22 code 
#e.g. main22 c1s1l1 sqrt
#this will produce a series of files in //arc//c1s1l1sqrt
#The typical format here is to source the R code containing the function
#and then call it repetitively on a series of files.
#Important part of code is reading in data from all lab.rep files once only and then
#passing the resulting massive R object to the functions. This takes lots of RAM but 
#speeds subsequent function execution from minutes to 3-4 seconds. 
#All of these functions assume that the lab.rep files will be read in using PBSModelling
#Thus library PBSmodelling required, available at http://code.google.com/p/pbs-software/downloads/list
#then in R go to menu Packages, Install Package from Local zip file.
#
#R code here programmed by Trevor A Branch in June and July 2009 tbranch@gmail.com
#Feel free to use and reuse at will; if included in a scientific publication, an acknowledgement 
#would be appreciated. 
#Dates: 10-20 July 2009
#######################################################################
library(PBSmodelling)   #for reading in the lab.rep files
library(gplots)         #for the rich.colors() function by Arni Magnusson

######################################################################
####Jim's scatterplot, showing the sampling of cells in a grid (uses .lev file)
######################################################################
source("ShadedPlots.r")
jpeg(file="figs\\ShadedPlot.jpg", width=600,height=600)
plot.lev("example\\arc\\c1s1l1\\C1S1L1.lev",title="Example shaded plot")
dev.off()

##########################################################################
#Get all the data into a single R object (large object). 
#Note that names(object) gives the file names of each object in the list
#Data in the list accessed by object[[1]]$Sbio, object[[2]]$M  etc. 
##########################################################################
source("get.all.data.r")
data.example.sqrt <- get.all.files(directory="example\\arc\\c1s1l1sqrt")
data.example.orig.5 <- get.all.files(directory="example\\arc\\c1s1l1orig.5")

######################################################
#Indonesian age fits
######################################################
Indo.fit <- function(data.objects) {
   nobjects <- length(data.objects)
   print(nobjects)
   for (i in 1:nobjects) {
      IndoAgeFits(data.object=data.objects[[i]],case_label=names(data.objects)[i])
   }
}
source("IndoAgeFits.r")
pdf("figs\\IndoAgeFits.pdf",width=6, height=11.5)
Indo.fit(data.objects=data.example.sqrt)
dev.off()


######################################################
#Table of NLL components, note that second last column is sum of 
#component NLLs and the final column is the sum of NLL plus sum
#of penalties (i.e. the objective function minimized in ADMB).
######################################################
source("TableLikelihoodComponents.r")
result <- likelihood.table(data.objects=data.example.sqrt)
write.csv(file="tables\\data.example.sqrt.csv",result)

######################################################
#Plots figure of numbers at age, Indonesian selectivity 
#and natural mortality (only works on a grid of 9 cells).
######################################################
#source("PlusGroup9by3.r")
#pdf("figs\\PreviousSqrt.pdf",width=11.5,height=6)
#compare.plus.groups2(directory="previoussqrt")
#dev.off()

######################################################
#Old template for looping through all files in order in a 
#directory and extracting e.g. tau or aerial sigma. 
#Much more easily and quickly done by looping through data.example.sqrt now though.
######################################################
#source("Array of values.r")
#cpue.sigma <- get.cpue.sigma(dir="sbtmod22\\arc\\C1S1L1orig.5\\",prefix="C1S1L1orig.5_")
#taus.orig.5 <- get.grid.taus(dir="m4est\\arc\\C1S1L1orig.5\\",prefix="C1S1L1orig.5_")
#hist(taus.orig.5)

######################################################
#Histograms of residuals
######################################################
source("SD residuals.r")
pdf(file="figs\\SD residuals.pdf",width=6,height=6)
hist.SD.resids(data.objects=data.example.sqrt,label="sqrt")
hist.SD.resids(data.objects=data.example.orig.5,label="orig.5")
dev.off()

######################################################
#plot.all.tagfits for one set of data
######################################################
plot.all.tagfits <- function(data.objects) {
   nobjects <- length(data.objects)
   print(nobjects)
   for (i in 1:nobjects) {
       tagging.fits(data.object=data.objects[[i]],case_label=names(data.objects)[i])
   }
}
source("Tagging fits combine tagger groups.r")
pdf("figs\\Example tagfits.pdf",width=6,height=8)
plot.all.tagfits(data.objects = data.example.sqrt)
dev.off()

######################################################
#Compare the tagging fits of two sets of grid cells (could be different models,
#or same model but different parameters, or two full grids etc.) 
######################################################
plot.all.pooled.tagfits <- function(data.compare.objects, compare.text="set1 set2") {
   ntocompare <- length(data.compare.objects) 
   for (i in 1:ntocompare) {
      nobjects <- length(data.compare.objects[[i]])
      print(nobjects)
   }
   
   for (i in 1:nobjects) {
       compare.objects <- list(data.compare.objects[[1]][[i]],data.compare.objects[[2]][[i]])
       label <- names(data.compare.objects[[1]])[i]
       label <- substr(label,nchar(label)-19,nchar(label)-8)
       case.labels <- paste(label,compare.text)
       tagging.fits.pooled.compare(compare.objects = compare.objects, case.label=case.labels)
   }
}
source("Compare tagging fits pooled.r")
pdf("figs\\Tagfits pooled compare.pdf",width=6,height=8)
data.compare.objects <- list(data.example.orig.5, data.example.sqrt)
plot.all.pooled.tagfits(data.compare.objects=data.compare.objects,compare.text="orig.5 sqrt")
dev.off()

######################################################
#Compare fits to data series between 2 cases
#Assumes two directories with identical file names in concordant positions
######################################################
compare.fits.all <- function(data.compare.objects, legend="set1 set2") {
   ntocompare <- length(data.compare.objects) 
   for (i in 1:ntocompare) {
      nobjects <- length(data.compare.objects[[i]])
      print(nobjects)
   }
   
   for (i in 1:nobjects) {
       compare.objects <- list(data.compare.objects[[1]][[i]],data.compare.objects[[2]][[i]])
       label <- names(data.compare.objects[[1]])[i]
       label <- substr(label,nchar(label)-19,nchar(label)-8)
       compare.fits(compare.objects = compare.objects, case_label=label,lab.cex=1.1,legend=legend)
   }
}
source("Compare fits.r")
pdf("figs\\CPUE aerial fits compare.pdf",width=5,height=7)
data.compare.objects <- list(data.example.orig.5, data.example.sqrt)
compare.fits.all(data.compare.objects=data.compare.objects,legend=c("orig.5","sqrt"))
dev.off()

######################################################
#Plot the recruitment and the recruitment deviates 
#for two different model runs.
######################################################
compare.rec.devs <- function(data.compare.objects,R.ylim=NULL,Rdev.ylim=NULL,legend) {
   ntocompare <- length(data.compare.objects) 
   for (i in 1:ntocompare) {
      nobjects <- length(data.compare.objects[[i]])
      print(nobjects)
   }
   for (i in 1:nobjects) {
     compare.objects <- list(data.compare.objects[[1]][[i]],data.compare.objects[[2]][[i]])
     label <- names(data.compare.objects[[1]])[i]
     label <- substr(label,nchar(label)-19,nchar(label)-8)
     compare.recruitment(compare.objects=compare.objects,R.ylim=R.ylim,Rdev.ylim=Rdev.ylim,label=label,legend=legend)
   }
}
source("Compare recruitment.r")
pdf("figs\\Compare recruitment.pdf",width=6,height=6)
data.compare.objects <- list(data.example.orig.5, data.example.sqrt)
compare.rec.devs(data.compare.objects=data.compare.objects,R.ylim=c(0,20),Rdev.ylim=c(-1.2,1.2),legend=c("orig.5","sqrt"))
dev.off()


