library(magrittr)

purrr::walk(paste0("R/",list.files("R")), source)

# Code for part 1
folder <- "day9"

rawData <- getInput(9,2022)
headMoves <- rawData$x

moveHead <- function(headPosition, instruction) {
  noOfSpaces <- as.numeric(stringr::str_sub(instruction, 3, nchar(instruction)))
  headx <- headPosition$headx
  heady <- headPosition$heady
  switch(
    stringr::str_sub(instruction, 1, 1),
    "R" = {
      headPositions <-
        data.frame(
          headx = headx + c(1:noOfSpaces),
          heady = rep(heady, noOfSpaces)
        )
    },
    "L" = {
      headPositions <-
        data.frame(
          headx = headx - c(1:noOfSpaces),
          heady = rep(heady, noOfSpaces)
        )
    },
    "U" = {
      headPositions <-
        data.frame(
          headx = rep(headx, noOfSpaces),
          heady = heady + c(1:noOfSpaces)

        )
    },
    "D" = {
      headPositions <-
        data.frame(
          headx = rep(headx, noOfSpaces),
          heady = heady - c(1:noOfSpaces)
        )
    }
  )

  headPositions
}

moveTail <- function(tailPosition, headx, heady) {
  difference <- c(headx, heady) - tailPosition
  if (all(abs(difference) <= 1)) {
    # no move so return current position
    return(tailPosition)
  }

  if (all(abs(difference) > 1)) {
    tailPosition <- tailPosition + difference / 2
  } else if (all(difference != 0)) {
    # diagonal move
    # find the direction of the 2
    position <- match(TRUE, abs(difference) == 2)
    tailPosition[position] <- tailPosition[position] + difference[position] / 2
    # other direction
    otherPos <- (position %% 2) + 1
    tailPosition[otherPos] <- tailPosition[otherPos] + difference[otherPos]

  } else {
    # straght line move
    tailPosition <- tailPosition + difference / 2
  }

  data.frame(tailx = tailPosition[1], taily = tailPosition[2])
}

# either straight line move in which case 1 of the value is the same
# or diagonal in which case both are different and the move is diagonal

headPosition <- data.frame(headx = 0, heady = 0)

for (i in 1:length(headMoves)) {

  headPosition <-
    dplyr::bind_rows(
      headPosition,
      moveHead(tail(headPosition, 1), instruction = headMoves[i])
    )
}

tailPosition <- data.frame(tailx = 0, taily = 0)
for (i in 1:nrow(headPosition)) {
  tailPosition <-
    dplyr::bind_rows(
      tailPosition,
      moveTail(tail(tailPosition,1), headPosition[i,1], headPosition[i,2])
    )
}

knotPaths <- list(head = headPosition, tail1 = tail(tailPosition, -1))

for (x in 2:9) {
  cat(paste("tail", x, "\n"))
  tailPosition <- data.frame(tailx = 0, taily = 0)
  for (i in 1:nrow(headPosition)) {
    tailPosition <-
      dplyr::bind_rows(
        tailPosition,
        moveTail(tail(tailPosition,1),knotPaths[[paste0("tail",x - 1)]][i,1], knotPaths[[paste0("tail",x - 1)]][i,2])
      )
  }
  knotPaths <-
    append(
      knotPaths,
      list(tail(tailPosition, -1))
    )
  names(knotPaths) <- c("head", paste0("tail", 1:x))

}

nrow(dplyr::distinct(knotPaths$tail1))
nrow(dplyr::distinct(knotPaths$tail9))
