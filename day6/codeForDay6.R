library(magrittr)

purrr::walk(paste0("R/",list.files("R")), source)

# Code for part 1
folder <- "day6"

rawData <- as.character(getInput(6,2022))
realData <- rawData

rawData <- "mjqjpqmgbljsphdztnvjfqwrcgsmlb"
positionFound <- FALSE
i <- 0
while (!positionFound) {
  i <- i + 1
  packet <- stringr::str_sub(realData, i, i + 13)

  if(length(unique(strsplit(packet, "")[[1]])) == 14) {
    positionFound <- TRUE
  }


}

packet
i + 13
