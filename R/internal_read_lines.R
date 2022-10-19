#' Read Lines Function for Stream Endpoints

#' This function reads lines from raw json files saved from the streaming endpoints
#' @param text file which neets to be read
#' @param fragment some option

#' @import data.table readr
#' @importFrom stats na.omit
#' @importFrom jsonlite fromJSON toJSON

#' @noRd
##################################################################################################
# Reads lines of results from API
##################################################################################################
whole_lines <- function(text, fragment = "") {
  lines <- strsplit(text, "\r\n")[[1]]
  lines[[1]] <- paste0(fragment, lines[[1]])

  n <- length(lines)
  complete <- grepl("\r\n$", text)
  if (!complete) {
    fragment <- lines[[n]]
    lines <- lines[-n]
  } else {
    fragment <- ""
  }

  # Drop empty keep-alive lines
  lines <- lines[lines != ""]

  list(lines = lines, fragment = fragment)
}
