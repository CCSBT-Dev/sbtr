#' Function to return version number
#'
.onAttach <- function(libname, pkgname)
{
    packageStartupMessage("sbtr version: 1.62
Compile date: 2019-07-03
")
}
