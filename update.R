#===============================================================
# UPDATE DESCRIPTION FILE
#===============================================================

DESCRIPTION <- readLines("DESCRIPTION")
VERSION <- as.numeric(gsub("Version: ", "", DESCRIPTION[2])) + 0.01
DESCRIPTION[2] <- paste("Version:", VERSION)
DATE <- Sys.Date()
DESCRIPTION[3] <- paste("Date:", DATE)
writeLines(DESCRIPTION, "DESCRIPTION")

# Write pkg.version()
filename <- "R/sbtr.version.R"
cat("#' Function to return version number\n", file = filename)
cat("#'\n", file = filename, append = TRUE)
cat(".onAttach <- function(libname, pkgname)\n", file = filename, append = TRUE)
cat("{\n", file = filename, append = TRUE)
cat(paste("    packageStartupMessage(\"sbtr version: ", VERSION, "\n", "Compile date: ", DATE, "\n\")\n", sep = ""), file = filename, append = TRUE)
cat("}\n", file = filename, append = TRUE)

#===============================================================

roxygen2::roxygenize()
