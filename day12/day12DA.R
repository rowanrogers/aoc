library(magrittr)

purrr::walk(paste0("R/",list.files("R")), source)


# Code for part 1
folder <- "day12"
source(file.path(folder,"djikstra.R"))

txt <- read.table(file.path(folder,"input.txt"))
lines <- purrr::map(txt,~stringr::str_split(., ""))

output <- c()
for (i in 1:length(lines[[1]])) {
  output <- c(output, lines[[1]][[i]])

}
mapOfCave <- matrix(output, ncol = length(lines[[1]][[i]]), byrow = T)

mapOfCave_graph <- createGraph(mapOfCave)

startPos <- match("S", mapOfCave)

endPos <- match("E", mapOfCave)

djSolved <- djikstra(mapOfCave_graph, startPos)

cat(paste0("Distance to finish is: ",djSolved$dist[endPos]))

# part 2
findBestStart(mapOfCave)




