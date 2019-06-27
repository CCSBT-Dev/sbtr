#' Function to return version number
#'
.onAttach <- function(libname, pkgname)
{
    packageStartupMessage("r_sbt version: 1.59
Compile date: 2019-06-28
")
}
