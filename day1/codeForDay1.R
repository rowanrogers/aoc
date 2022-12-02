library(magrittr)

purrr::walk(paste0("R/",list.files("R")), source)

folder <- "day1"

input <- getInput(1,2022)

naCalc <-
  input %>%
  dplyr::rowwise() %>%
  dplyr::mutate(naCount = ifelse(is.na(.data$x), 1, 0)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(elfNumber = cumsum(.data$naCount) + 1)

calCount <-
  naCalc %>%
  dplyr::group_by(.data$elfNumber) %>%
  dplyr::summarise(calorieCount = sum(.data$x, na.rm = TRUE)) %>%
  dplyr::arrange(dplyr::desc(.data$calorieCount))


sum(calCount$calorieCount[1:3])
