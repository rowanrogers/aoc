library(magrittr)

purrr::walk(paste0("R/",list.files("R")), source)

folder <- "day1"

input <- getInput(2,2022)
# Score from my entry - X == 3, Y == 2, Z == 1
# Score from combos - X
#
#
playCalc <- function(p2) {

  switch(
    p2,
    "X" = 1,
    "Y" = 2,
    "Z" = 3
  )

}


playCalcStrat2 <- function(p1, p2) {
  # convert
  p1_ <- switch(p1, "A" = "rock", "B" = "paper", "C" = "scissors")
  p2_ <- switch(p2, "X" = "lose", "Y" = "draw", "Z" = "win")
  # score
  if (p1_ == "rock") {
    switch(p2_,
           "lose" = 3,
           "draw" = 1,
           "win" = 2)
  } else if (p1_ == "paper") {
    switch(p2_,
           "lose" = 1,
           "draw" = 2,
           "win" = 3)
  } else if (p1_ == "scissors") {
    switch(p2_,
           "lose" = 2,
           "draw" = 3,
           "win" = 1)
  }

}

scoreCalc <- function(p1, p2) {
  # convert
  p1_ <- switch(p1, "A" = "rock", "B" = "paper", "C" = "scissors")
  p2_ <- switch(p2, "X" = "rock", "Y" = "paper", "Z" = "scissors")
  # score
  if (p1_ == "rock") {
    switch(p2_,
           "rock" = 3,
           "paper" = 6,
           "scissors" = 0)
  } else if (p1_ == "paper") {
    switch(p2_,
           "rock" = 0,
           "paper" = 3,
           "scissors" = 6)
  } else if (p1_ == "scissors") {
    switch(p2_,
           "rock" = 6,
           "paper" = 0,
           "scissors" = 3)
  }
}

scoreCalcStrat2 <- function(p2) {
  # convert
  switch(p2, "X" = 0, "Y" = 3, "Z" = 6)

}


combs <- expand.grid(p1 = c("A", "B", "C"), p2 = c("X", "Y", "Z")) |>
  dplyr::rowwise() |>
  dplyr::mutate(
    p1 = as.character(.data$p1),
    p2 = as.character(.data$p2),
    playPoints = as.integer(playCalc(.data$p2)),
    compPoints = as.integer(scoreCalc(.data$p1, .data$p2)),
    playPtsStrat2 = playCalcStrat2(.data$p1, .data$p2),
    compPtsStrat2 = scoreCalcStrat2(.data$p2)
  )


analysis <- "day2"
rawdata <-
  input |>
  dplyr::rowwise() |>
  dplyr::mutate(
    V1 = stringr::str_split(.data$x, pattern = " ")[[1]][1],
    V2 = stringr::str_split(.data$x, pattern = " ")[[1]][2]
  ) |>
  dplyr::transmute(player1 = .data$V1, player2 = .data$V2)

mergedData <-
  dplyr::full_join(rawdata, combs, by = c("player1" = "p1", "player2" = "p2"))

sum(mergedData$playPtsStrat2 + mergedData$compPtsStrat2)




