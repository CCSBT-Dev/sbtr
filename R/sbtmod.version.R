#' Function to return version number
#'
.onAttach <- function(libname, pkgname)
{
    packageStartupMessage("sbtmod version: 1.6
Compile date: 2019-06-28
")
}
