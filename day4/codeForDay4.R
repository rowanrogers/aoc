library(magrittr)

purrr::walk(paste0("R/",list.files("R")), source)

# Code for part 1
folder <- "day4"

rawData <- getInput(4,2022)

neatData <- stringr::str_extract_all(rawData$x,"[0-9]+")

# part 1
containedRanges <-
  purrr::map_lgl(
    neatData,
    function(x) {
      x <- as.numeric(x)
      all((x[1]:x[2]) %in% (x[3]:x[4])) || all((x[3]:x[4]) %in% (x[1]:x[2]))

    }
  )

sum(containedRanges)

# part 2
overlapRanges <-
  purrr::map_lgl(
    neatData,
    function(x) {
      x <- as.numeric(x)
      any((x[1]:x[2]) %in% (x[3]:x[4])) || any((x[3]:x[4]) %in% (x[1]:x[2]))

    }
  )

sum(overlapRanges)
