#' Streaming Function for Stream Endpoints

#' This function initiates the collection of tweets using either the sampled stream or the search stream endpoint
#' @param stream curl element to connect to stream
#' @param output output file name and path
#' @param append logical if one wants to add stream to existing file
#' @param timeout time in seconds the stream should run
#' @param verbose logical to show progress

#' @import httr httpuv RCurl ROAuth data.table readr
#' @importFrom stats na.omit
#' @importFrom lubridate as_datetime
#' @importFrom jsonlite fromJSON toJSON
#' @importFrom dplyr bind_rows
#' @importFrom progress progress_bar
#' @importFrom curl curl
#' @importFrom rlang has_name
#' @importFrom withr defer

#' @noRd
##################################################################################################
# Download Data from Stream Function
##################################################################################################
download_from_stream <- function(stream, output, append = TRUE, timeout = 30, verbose = TRUE) {
  if (!timeout) {
    timeout <- Inf
  }
  stopifnot(is.numeric(timeout), timeout > 0)
  stop_time <- Sys.time() + timeout

  n_seen <- 0
  if (verbose) {
    pb <- progress::progress_bar$new(
      total = NA,
      show_after = 0,
      format = "Streaming tweets: :n tweets written / :bytes / :rate / :elapsedfull"
    )
  }

  open(stream, "rb")
  withr::defer(close(stream), rlang::current_env(), priority = "first")

  open(output, if (append) "ab" else "b")
  withr::defer(close(output), rlang::current_env(), priority = "last")

  lines <- list(lines = character(), fragment = "")
  while (isIncomplete(stream) && Sys.time() < stop_time) {
    buf <- readBin(stream, raw(), 64 * 1024)
    if (length(buf) == 0) {
      if (verbose()) {
        pb$tick()
      }
      Sys.sleep(0.25)
      next
    }

    text <- rawToChar(buf)
    lines <- whole_lines(text, lines$fragment)

    # only keep tweets
    json <- lapply(lines$lines, jsonlite::fromJSON, flatten = T)
    is_tweet <- vapply(json, function(x) rlang::has_name(x, "data"), logical(1))

    n_seen <- n_seen + sum(is_tweet)
    if (verbose) {
      pb$tick(length(buf), tokens = list(n = n_seen))
    }

    writeLines(lines$lines[is_tweet], output, useBytes = TRUE) # useBytes = TRUE results in lexical errors with R 4.1 and earlier ... iconv()
  }

  if (verbose) {
    cat("\n")
  }

  invisible()
}
