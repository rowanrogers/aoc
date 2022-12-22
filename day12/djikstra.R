djikstra <- function(mapOfCave_graph, source) {

  dist <- c() # distance of vertices from source
  prev <- c()
  Q <- c()

  for (i in 1:ncol(mapOfCave_graph)) {
    dist[i] <- Inf
    prev[i] <- NA
    Q[i] <- i
  }

  dist[source] <- 0
  names(dist) <- 1:ncol(mapOfCave_graph)
  while (length(Q) > 0) {

    u <- names(which.min(dist[Q])) %>% as.numeric()
    Q <- Q[Q != u]

    neighbours <- which(mapOfCave_graph[,u] > 0)
    neighbours <- neighbours[neighbours %in% Q]
    for (j in neighbours) {
      # 1 here is distance
      alt <- dist[u] + mapOfCave_graph[j,u]
      if (alt < dist[j]) {
        dist[j] <- alt
        prev[j] <- u
      }

    }
  }
  return(list(dist = dist, prev = prev))

}

# finds positions of valid moves based on location and values
findValidMoves <- function(i, mapOfCave) {

  posL <- mapOfCave[i]

  validNextStep <- checkValidMoves(posL)

  # find the locations of valid moves
  validMoves <- checkLocations(i, mapOfCave, validNextStep)

  validMoves
}


# Helpr
# finds the letters you are able to move to given current letter
checkValidMoves <- function(pos) {
  if (pos == "S") {
    validNextStep <- "a"
  } else if (pos == "E") {
    validNextStep <- letters
  } else if (pos == "z") {
    validNextStep <- c(letters, "E")
  } else {
    posL <- match(pos, letters)

    validNextStep <- letters[1:(posL + 1)]
  }
  validNextStep
}


# creates a matrix of distances of valid paths between nodes
createGraph <- function(mapOfCave) {
  mapOfCave_graph <- c()

  pb <- txtProgressBar(0,1)

  for(i in 1:length(mapOfCave)) {
    setTxtProgressBar(pb, value = i / length(mapOfCave))
    validMoves <- findValidMoves(i, mapOfCave)

    graphCol <- rep(0,length(mapOfCave))
    graphCol[validMoves] <- 1

    mapOfCave_graph <- cbind(mapOfCave_graph, graphCol)

  }

  colnames(mapOfCave_graph) <- 1:ncol(mapOfCave_graph)

  mapOfCave_graph
}



checkLocations <- function(pos, mapOfCave, validLs) {

  n <- nrow(mapOfCave)

  validPositions <- c()

  if (pos + n <= length(mapOfCave)) {
    validPositions <- c(validPositions, pos + n)
  }
  if (pos - n > 0) {
    validPositions <- c(validPositions, pos - n)
  }

  if ((pos - 1) %/% n + 1 == (pos %/% n + 1)) {
    validPositions <- c(validPositions, pos + 1)
  }

  if (((pos - 2) %/% n + 1) == ((pos - 1) %/% n + 1)) {
    validPositions <- c(validPositions, pos - 1)
  }

  validMove <- c()

  for (i in validPositions) {
    # make sure position is on the map
      # check whether we are able to move to i
    if (mapOfCave[i] %in% validLs) {
      validMove <- c(validMove,i)
    }
  }

  validMove

}



findBestStart <- function(mapOfCave) {

  pb <- txtProgressBar(0,1)
  shortestRoute <- c()

  for(S in c(1:length(mapOfCave))[mapOfCave == "b"]) {

    setTxtProgressBar(pb, value = S / length(c(1:length(mapOfCave))[mapOfCave == "b"]))

    djSolved <- djikstra(mapOfCave_graph, S)
    shortestRoute[S] <- djSolved$dist[endPos]
  }

  shortestRoute
}
