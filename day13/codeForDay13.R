library(magrittr)

purrr::walk(paste0("R/",list.files("R")), source)


# Code for part 1
folder <- "day13"

txt <- read.table(file.path(folder,"input.txt"))

convertToRList <- function(string) {

  string <-
    stringr::str_replace_all(
      string,
      pattern = "\\[",
      replacement = "list("
    ) |>
    stringr::str_replace_all(
      pattern = "\\]",
      replacement = ")"
    ) %>% parse(text = .) %>% eval()

  string
}

# compares packets once in list format
compareLists <- function(pack1, pack2) {

  maxLength <- max(length(pack1), length(pack2))
  for(i in 1:maxLength) {
    p1 <- purrr::pluck(pack1, i)
    p2 <- purrr::pluck(pack2, i)


    if (rlang::is_null(p1) && rlang::is_null(p2)) {
      next
    } else if (rlang::is_null(p1) && !rlang::is_null(p2)) {
      return(TRUE)
    } else if (!rlang::is_null(p1) && rlang::is_null(p2)) {
      return(FALSE)
    }

    if(is.list(p1) || is.list(p2)) {
      p1 <- as.list(p1)
      p2 <- as.list(p2)
      output <- compareLists(p1, p2)
      if(is.logical(output)) {return(output)} else {next}
    } else if (is.numeric(p1) && is.numeric(p2)) {
      if (p1 < p2) {return(TRUE)} else if (p1 > p2) {return(FALSE)}
    }

  }

  return(NULL)

}

n <-(nrow(txt)/2)
results <- c()
inputList <- list()
for(i in 1:n) {

  packets <- txt[c(2 * i - 1, 2 * i),]

  pack1 <- convertToRList(packets[1])
  pack2 <- convertToRList(packets[2])
  inputList <- append(inputList, list(pack1)) %>% append(list(pack2))
  x <- compareLists(pack1, pack2)

  results[i] <- x


}


paste0("Part 1: ",sum((1:n)[results]))

pluckFirst <- function(x) {

  firstItem <- purrr::pluck(x, 1)
  if(is.list(firstItem)) {
    firstItem <- pluckFirst(firstItem)
  }

  if(rlang::is_null(firstItem)) { firstItem <- 0 }
  data.frame(firstItem = firstItem)

}

ordering <- purrr::map_dfr(inputList, pluckFirst) %>%
  dplyr::arrange(firstItem)

twoPos <- min(which(ordering[,1] == 2))
sixPos <- min(which(ordering[,1] == 6)) + 1

paste0("Part 2: ", twoPos * sixPos)





