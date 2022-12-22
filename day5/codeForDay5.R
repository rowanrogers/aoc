library(magrittr)

purrr::walk(paste0("R/",list.files("R")), source)

# Code for part 1
folder <- "day5"

rawData <- getInput(5,2022)

View(rawData)

crates <- rawData[1:8,]

# first want to get the crate stacks into a workable list
sortedCrates <-
  crates |>
  dplyr::rowwise() |>
  dplyr::mutate(formattedCrate = list(purrr::map_chr(1:9, ~substr(.data$x, 4*(.x-1)+2, 4*(.x-1)+2)))) |>
  tidyr::unnest(formattedCrate) |>
  dplyr::ungroup() |>
  dplyr::mutate(
    stackId = ((dplyr::row_number() - 1) %% 9) + 1
  )

# now have the crates as identifiable objects and locations, next step is to
# convert to a nice character list so we can perform operations
cratesList <-
  purrr::set_names(1:9) |>
  purrr::map(
    .f = function(i) {
      orderedStack <-
        sortedCrates |>
        dplyr::filter(.data$stackId == i) |>
        dplyr::pull(.data$formattedCrate) |>
        rev()

      orderedStack[!orderedStack %in% c(" ", "")]
    }
  )

# now we have the crates in a nice shape we can apply to operations.
tidyInstructions <-
  rawData[-(1:10),] |>
  dplyr::mutate(
    instructionNumber = dplyr::row_number()
  ) |>
  dplyr::rowwise() |>
  dplyr::mutate(
    op = stringr::str_extract_all(string = .data$x, pattern = "[0-9]+")
  ) |> tidyr::unnest_wider(op, names_sep = "_") |>
  dplyr::mutate(
    op_1 = as.numeric(op_1),
    op_2 = as.numeric(op_2),
    op_3 = as.numeric(op_3)
  )


performOp <- function(op1, op2, op3, crates, reverse = FALSE) {
  if(reverse) {
    cratesToMove <- tail(crates[[op2]], op1)
  } else {
    cratesToMove <- rev(tail(crates[[op2]], op1))
  }

  cratesToMove <- tail(crates[[op2]], op1)
  #remove crates from initial stack
  crates[[op2]] <- head(crates[[op2]], -op1)
  # add crates
  crates[[op3]] <- c(crates[[op3]], cratesToMove)

  crates
}

cratesSorted <- cratesList

for (i in 1:nrow(tidyInstructions)) {

  cratesSorted <-
    performOp(
      tidyInstructions$op_1[i],
      tidyInstructions$op_2[i],
      tidyInstructions$op_3[i],
      cratesSorted
    )

}


# find top of each column
purrr::map_chr(cratesSorted, ~tail(.,1)) |> paste0(collapse = "")

