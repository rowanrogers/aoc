library(magrittr)

purrr::walk(paste0("R/",list.files("R")), source)

# Code for part 1
folder <- "day7"

rawData <- getInput(7,2022)
terminalOutput <- rawData$x

# outer most directory
# $ cd /
# $ cd x moves to x
# $ cd .. moves up one level
# $ ls lists the files/ directories

# helper functions
upLevel <- function(x) {
  finalFolderLoc <- as.numeric(tail(stringr::str_locate_all(x, "/")[[1]], 1)[1] - 1)
  stringr::str_sub(x, 1, finalFolderLoc)
}

inLevel <- function(x, folderName) {
  paste0(x, "/", folderName)
}

#data structure
currentFolder <- "/"

# define output structure
output <- data.frame(position = character(), size = numeric())

## this creates an object with correct structure and size
for (i in 1:length(terminalOutput)) {

  if (terminalOutput[i] == "$ cd /") {
    # reset file dir
    currentFolder <- ""

  } else if (terminalOutput[i] == "$ cd ..") {
    # go up a level
    currentFolder <- upLevel(currentFolder)

  } else if (stringr::str_detect(terminalOutput[i], "\\$ cd ")) {
    # go down a level
    folderName <- stringr::str_sub(terminalOutput[i], start = 6, nchar(terminalOutput[i]))
    currentFolder <- inLevel(currentFolder, folderName)

  } else if (stringr::str_detect(terminalOutput[i], "dir ")) {
    # create a folder (of size 0) in the output
    folderName <- stringr::str_sub(terminalOutput[i], start = 5, nchar(terminalOutput[i]))

    output <-
      dplyr::bind_rows(
        output,
        data.frame(position = paste0(currentFolder, "/", folderName), size = 0)
      )

  } else if (stringr::str_detect(terminalOutput[i], "[:digit:]+.")) {
    # create a file of required size in the output
    size <- as.numeric(stringr::str_extract(terminalOutput[i], "[:digit:]+"))
    dataLocation <- paste0(currentFolder, "/", stringr::str_extract(terminalOutput[i], "[:alpha:]+"))
    output <-
      dplyr::bind_rows(
        output,
        data.frame(position = dataLocation, size = size)
      )

  }
}

# all folders have size 0 so we can determine all the folders.
allFolders <- dplyr::filter(output, .data$size < 0.001)$position %>% purrr::set_names()

# now we can sum over all folders
sizeOfFolders <-
  purrr::map_dbl(
    allFolders,
    .f = function(x) {
      sum(output$size[grepl(x, output$position)])
    }
  )

# part 1
sum(sizeOfFolders[sizeOfFolders <= 100000])


# space used
spaceUsed <- sum(output$size)

# space required
spaceRequired <- spaceUsed - 40000000

min(sizeOfFolders[sizeOfFolders > spaceRequired])
