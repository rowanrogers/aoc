## Read in the daily input

getInput <- function(day, year) {
  session <- Sys.getenv("ADVENT_SESSION")
  url <- paste0("https://adventofcode.com/", year, "/day/", day, "/input")

  req <-
    httr::GET(
      url,
      httr::set_cookies(session = session))

  httr::stop_for_status(req)

  txt <- httr::content(req, encoding = "UTF-8")

  lines <- stringr::str_split(txt, "\n")[[1]]

  lines <- readr::parse_guess(lines, locale = readr::locale("en", grouping_mark = ""))

  tibble::tibble(x = head(lines, -1))

}
