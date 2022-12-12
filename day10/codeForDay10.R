library(magrittr)

purrr::walk(paste0("R/",list.files("R")), source)

# Code for part 1
folder <- "day10"

rawData <- getInput(10,2022)
terminalOutput <- rawData$x

testData <- read.csv(file.path(folder, "testData.csv"))

## improved version
cycleNumber <- 0
regValue <- 1

status <-
  data.frame(
    cycleNumber = cycleNumber,
    regValue = regValue
  )

for (i in 1:length(rawData$x)) {

  if (grepl("noop",rawData$x[i])) {
    cycleNumber <- cycleNumber + 1
    regValue <- regValue

    status <-
      dplyr::bind_rows(
        status,
        data.frame(
          cycleNumber = cycleNumber,
          regValue = regValue
        )
      )
  } else {
    cycleNumber <- cycleNumber + 1

    status <-
      dplyr::bind_rows(
        status,
        data.frame(
          cycleNumber = c(cycleNumber, cycleNumber + 1),
          regValue = c(regValue, regValue)
        )
      )
    cycleNumber <- cycleNumber + 1
    regValue <- regValue + as.numeric(stringr::str_sub(rawData$x[i], 6))
  }

}

# part 1
dplyr::filter(status, .data$cycleNumber %in% c(20,60,100,140,180,220)) %>%
  dplyr::mutate(totalValue = .data$regValue * .data$cycleNumber) %>%
  dplyr::pull(totalValue) %>% sum


# part 2
status <-
  status %>%
  dplyr::rowwise() %>%
  dplyr::mutate(
    spritePosition = list(c(.data$regValue - 1, .data$regValue, .data$regValue + 1)),
    pixelPosition = (.data$cycleNumber - 1) %% 40,
    pixelDraw = ifelse(.data$pixelPosition %in% .data$spritePosition, "#", ".")
  )

pixelDraw <- tail(status %>% dplyr::pull(.data$pixelDraw), -1)

# write letters to console
for (i in 1:length(pixelDraw)) {
  cat(pixelDraw[i])
  if (i %% 40 - 0 < .Machine$double.eps) {
    cat("\n")
  }
}

