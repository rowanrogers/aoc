library(magrittr)

purrr::walk(paste0("R/",list.files("R")), source)

# Helper functions
findChars <- function(string1, string2) {
  charLocator <- FALSE
  i <- 0
  while (charLocator == FALSE) {
    i <- i + 1
    charToFind <- stringr::str_sub(string1, start = i, end = i)
    charLocator <- stringr::str_detect(string = string2, charToFind)
  }

  charToFind
}

findAllChars <- function(string1, string2) {
  charLocator <- character()
  for (i in 1:nchar(string1)) {
    charToFind <- stringr::str_sub(string1, start = i, end = i)
    charLocator[i] <- as.character(stringr::str_match(string = string2, charToFind))
  }
  paste0(unique(charLocator[!is.na(charLocator)]),collapse = "")

}

# Code for part 1
folder <- "day3"

# Can use position in vector to get the priority
symbols <- c(letters,LETTERS)

rawData <- getInput(3,2022)

augmentedData <-
  rawData |>
  dplyr::rowwise() |>
  dplyr::mutate(
    firstComp =
      stringr::str_sub(.data$x,start = 1, end = nchar(.data$x)/2),
    lastComp =
      stringr::str_sub(
        .data$x,
        start = nchar(.data$x)/2 + 1,
        end = nchar(.data$x)
      ),
    matchedChar = findChars(.data$firstComp, .data$lastComp)
  )

sum(match(augmentedData$matchedChar, symbols))

# Code for part 2
groupedData <-
  rawData |>
    dplyr::mutate(
     group = dplyr::row_number() %% 3,
     elfNo = (dplyr::row_number() - 1) %/% 3
  ) |>
  tidyr::pivot_wider(values_from = x, names_from = group, names_glue = "group{group}") |>
  dplyr::rowwise() |>
  dplyr::mutate(
    firstMatch = findAllChars(.data$group0, .data$group1),
    badgeId = findChars(.data$firstMatch, .data$group2)
  )


sum(match(groupedData$badgeId, symbols))







