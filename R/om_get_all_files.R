#' Load _lab.rep files into R
#'
#' Goes to the specified directory and finds all the _lab.rep files. Loads them one by one into an R list of objects. This ensures that the reading in stage (which can take 3-10 minutes) need only be done once, and then subsequent function calls looping through an entire grid will run in a few seconds instead of minutes. Required the following files: something ending in "_lab.rep", assuming that the naming convention in the file has sections of outputs separated by $name comments so that it can be easily parsed by readList() function of Jon Schnute.
#'
#' @param directory the directory that the _lab.rep files are in
#' @return a list of objects
#' @examples
#' data.example.sqrt <- get_all_files(directory = "arc/c1s1l1sqrt/")
#' @importFrom PBSmodelling readList
#' @export
#'
get_all_files <- function(directory) {
    files <- dir(path = directory, pattern = "lab.rep")
    # files <- list.files(path = directory, pattern = "lab.rep")
    nn <- length(files)
    result <- list()
    for (i in 1:nn) {
       print(paste(i, "of", nn))
       result[[i]] <- readList(fname = file.path(directory, files[i]))
       names(result)[i] <- files[i]
    }
    return(result)
}
