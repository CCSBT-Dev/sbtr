#################################################
#Goes to the specified directory and finds all the _lab.rep files.
#Loads them one by one into a massive R list of objects, which takes time.
#Returns the list. This ensures that the reading in stage (which can take 3-10 minutes)
#need only be done once, and then subsequent function calls looping through 
#an entire grid will run in a few seconds instead of minutes. 
#Trevor A. Branch (assistance from Ana Parma) July 2009 
#################################################
get.all.files <- function(directory) {
    library(PBSmodelling)
    files<-dir(directory,pattern="lab.rep")
    nn = length(files)

    result <- list()
    for (i in 1:nn) {
       print(paste(i,"of",nn))
       result[[i]] <- readList(paste(directory,"\\",files[i],sep=""))     
       names(result)[i] <- files[i]
    }
    
    return(result)
}
#data.example.sqrt <- get.all.files(directory="example\\arc\\c1s1l1sqrt")


