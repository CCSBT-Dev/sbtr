# Package sbtmod

The package sbtmod is an R package for manipulating and plotting Southern bluefin tuna (SBT) model outputs.

## Installing sbtmod

The `sbtmod` package can be built and installed using:

    make

from within the `sbtmod/` directory (or similar depending on your operating system).

## Using sbtmod

Once installed, the sbtmod package can be called in R using `library(sbtmod)`.

All figures produced during the 23-26 July 2013 meeting in Portland, Maine, are contained in the file `2013_07 Portland meeting scripts.r`. Typically how the code is organized is with a directory containing all the .r code, and subdirectories `\figs` where the .pdf figures are saved, `\levfiles` containing the .lev files, `\gridfiles` containing the .grid files, `\tables` where the .csv outputs are saves. One final directory, the `\arc` directory, contains a subdirectory for each set of model evaluations, including the all-important lab.rep files, e.g. `\arc\base2010sqrtns\base2010sqrtns_h1m1M1O2C2a1_lab.rep`.     

The code works by first reading in all of the lab.rep files for a particular model run (all lab.rep files in a directory) and saving these in a list, e.g. `data.base2010ns`. This takes a few minutes. All subsequent code uses these lists and runs quickly (1-5 seconds).     

There is a .zip file containing all lab.rep and associated files, and a .zip file containing just the .r files.     

The word file in the R directory provides further details.

### Examples

Examples input files are provided in `arc/example_lab.rep` and `levfiles/example.lev`. The `_lab.rep` input file can be read into R using:

    data.example <- get.all.files("arc/example_lab.rep")

Reading `_lab.rep` files takes a long time so it is recommended to save the data object as an `.RData` file or similar once read in. Now tables and plots can be produced as desired. For example:

    nll.table.example <- likelihood.table2(data.objects = data.example)
    write.csv(file = "tables/exampleNLL.csv", nll.table.example)

    pdf("figs/Fig 1 NLLs example.pdf")
    Plot.NLL.by.steepness(nll.table.example, caption = "example")
    dev.off()

Alternatively, one can plot `.lev` files using:

    jpeg(file = "figs/Fig 5 Shaded base.jpg", width = 600, height = 600)
    Plot.lev("levfiles/example.lev")
    dev.off()

## Adding R functions to the rsbt package

The sbtmod source code is located in the directory `sbtmod/R/`. Additional R functions can be added to the package by placing them in the `/R/` directory. Generally a single R function should be included in a file with the naming convension `function_name.R`. For a function to be included in the package build a roxygen header must be added above each new R function. This header and function should be formatted as follows:

    #' Function title
    #' 
    #' Description of function
    #' 
    #' @param y the function input
    #' @return something done on y
    #' @author John Doe
    #' @examples
    #' x <- function_name(y) # An example of how to run the function
    #' @export
    #'
    function_name <- function(y)
    {
        out <- do_something_to(y)
        return(out)
    }

The package can be compiled using the `Makefile` by simply typing `make`. This builds the `.Rd` helpfiles and the `NAMESPACE` file within the package source directories and then builds and installs the package.

## Script for covariance function

To evaluate the within-cell variabilty in addition to the structural 
accross-the-grid uncertainty

### Preliminaries: 
May need to edit main.tpl so that -est in call to sbtmod is removed
This needs to be added as a flag rather than hardwired as computing the
Hessian each run adds time to evaluating the grid

E.g., within directory OM run:
 ` main base2013 sqrt`

  to create the base2013sqrt subdirectory within OM/arc and save labrep files
  AND now the .cor .std and .par files that can be used for further evaluation

  Below are the scripts that evaluate these results further

=====Required packages==============            
`install.packages("PBSmodelling")  `
`require(PBSmodelling)      `
`library(rsbt)          `

Edit this to your needs...
`setwd("/Users/jim/_mymods/sbtmod/runs")`

 These two lines take time...1st reads in all results, second reads in and computes
 covariances
`   labrep <- get.all.files("arc/base2013sqrt")`      
`   covrep <- get.cov.est()`      
 To store your R environment (and avoid loading ASCII each time)      
`   save.image(file="My.RData")`      
 if your data had been stored in R:            
`load("My.RData")`

Use covariance to generate B10 and F simulations of posterior over each point in grid
 Also includes terminal year numbers at age estimates and covariances..
`sims  <- sim.F.B10(covrep,lev.file="../OM/arc/grids/base2013sqrt.lev")`
`lev.file="../OM/arc/grids/base2013sqrt.lev"`

-----Plot comparisons of within cell variability vs grid---------------    
`MSYcov.stats  <- Plot.MSY.cov(labrep,lev.file,sims,xlim=c(0,2.3))`
