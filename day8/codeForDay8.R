library(magrittr)

purrr::walk(paste0("R/",list.files("R")), source)

# Code for part 1
folder <- "day8"

rawData <- getInput(8,2022)

x <- purrr::map(
  rawData,
  ~stringr::str_split(., "")
)

# numeric matrix
xNum <- matrix(unlist(purrr::map(x$x, as.numeric)), nrow = 99)
results <- list()

# helpr functions
calcScore <- function(value, treeline) {
  min(match(TRUE, c(treeline, 100) >= value), length(treeline))
}

calcVisibility <- function(i, j, dataframe) {
  x <- dataframe[i, j]

  # look up
  treeLineUp <- rev(dataframe[1:(i - 1), j])
  scoreUp <- calcScore(x, treeLineUp)
  # look right
  treeLineRight <- dataframe[i, (j + 1):ncol(dataframe)]
  scoreRight <- calcScore(x, treeLineRight)
  # look down
  treeLineDown <- dataframe[(i + 1):nrow(dataframe), j]
  scoreDown <- calcScore(x, treeLineDown)
  # look left
  treeLineLeft <- rev(dataframe[i, 1:(j - 1)])
  scoreLeft <- calcScore(x, treeLineLeft)

  totalScore <- scoreUp * scoreRight * scoreDown * scoreLeft
  totalScore
}


## now test each tree
for (i in 1:nrow(xNum)) {
  for (j in 1:ncol(xNum)) {
    if (i == 1 || j == 1 || i == nrow(xNum) || j == ncol(xNum)) {
      results <-
        append(
          results,
          list(
            list(
              value = xNum[i,j],
              position = c(i,j),
              score = 0,
              visible = TRUE
            )
          )
        )
    } else {
      #test left
      test <-
        (xNum[i,j] > max(xNum[i, 1:(j - 1)]) ||
           # test right
           xNum[i,j] > max(xNum[i, (j + 1):ncol(xNum)]) ||
           # test up
           xNum[i,j] > max(xNum[1:(i - 1), j]) ||
           # test down
           xNum[i,j] > max(xNum[(i + 1):nrow(xNum), j]))

      if (test) {
        results <-
          append(
            results,
            list(
              list(
                value = xNum[i,j],
                position = c(i,j),
                score = calcVisibility(i, j, xNum),
                visible = TRUE
              )
            )
          )
      } else {
        results <-
          append(
            results,
            list(
              list(
                value = xNum[i,j],
                position = c(i,j),
                score = calcVisibility(i, j, xNum),
                visible = FALSE
              )
            )
          )
      }

    }
  }
}

# part 1 is to find all visible trees
visibility <- purrr::map_lgl(results, ~purrr::pluck(., "visible"))
sum(visibility)

# part 2 is to find max score
scores <- purrr::map_dbl(results, ~purrr::pluck(.,"score"))
max(scores)
