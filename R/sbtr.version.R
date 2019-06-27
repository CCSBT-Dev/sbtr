#' Function to return version number
#'
.onAttach <- function(libname, pkgname)
{
    packageStartupMessage("sbtr version: 1.61
Compile date: 2019-06-28
")
}
