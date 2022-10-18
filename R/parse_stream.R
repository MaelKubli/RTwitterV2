#' Converts Twitter stream data (JSON file) into parsed data frame.
#' @param path Character, name of JSON file with data collected by
#'   [stream_tweets()].
#' @param filtered logical, indicating whether the stream to parse is filtered or not. Defaults to FALSE. This will add the filter rule variables to the data
#'
#' @return a data frame
#' @export
#'
#' @examples
#' \dontrun{
#' stream_tweets(timeout = 1, file_name = "stream.json", parse = FALSE)
#' parse_stream("stream.json")
#' }
#'
#' @import httr httpuv RCurl ROAuth data.table readr
#' @importFrom lubridate as_datetime
#' @importFrom jsonlite fromJSON toJSON
#' @importFrom dplyr bind_rows
##################################################################################################
# Loads Parser Function
##################################################################################################
parse_stream <- function(path, filtered = F) {

  if (!is(path, "file")) {
    path <- file(path)
  }

  tweets <- jsonlite::stream_in(path, pagesize = 1, verbose = T, simplifyVector = FALSE, flatten = FALSE)
  tweets <- data_parser_stream(results_data = tweets, filtered = filtered)
  return(tweets)
}
