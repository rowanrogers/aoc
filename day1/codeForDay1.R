library(magrittr)


## load helper functions for fetching data and parsing
purrr::walk(paste0("R/",list.files("R")), source)

# specify folder
folder <- "day1"

# load input from website
input <- getInput(1,2022)

# mutate to define which calorie count is related to each elf
naCalc <-
  input |>
  dplyr::rowwise() |>
  dplyr::mutate(naCount = ifelse(is.na(.data$x), 1, 0)) |>
  dplyr::ungroup() |>
  dplyr::mutate(elfNumber = cumsum(.data$naCount) + 1)

# sum over the calories for each elf
calCount <-
  naCalc |>
  dplyr::group_by(.data$elfNumber) |>
  dplyr::summarise(calorieCount = sum(.data$x, na.rm = TRUE)) |>
  dplyr::arrange(dplyr::desc(.data$calorieCount))

# find max calorie count
max(calCount$calorieCount)
# find sum of top 3 calorie counts
sum(calCount$calorieCount[1:3])
