# ' @title: RTweetV2 Function collecting tweets from the filterd stream

#' This function allows you to collect tweets from the 1 \% sampled stream from twitter
#' @param token string of the bearer token from your personal twitter API access
#' @param timeout numeric scalar specifying amount of time, in seconds, to leave connection open while streaming/capturing tweets. By default, this is set to 30 seconds. The maximum duration for an open stream is set to 300 seconds to avoid large json files. .
#' @param backfill By passing this parameter, you can request up to five (5) minutes worth of streaming data that you might have missed during a disconnection to be delivered to you upon re-connection. The back filled Tweets will automatically flow through the reconnected stream, with older Tweets generally being delivered before any newly matching Tweets. You must include a whole number between 1 and 5 as the value to this parameter. This feature will deliver duplicate Tweets, meaning that if you were disconnected for 90 seconds, and you requested two minutes of backfill, you will receive 30 seconds worth of duplicate Tweets. Due to this, you should make sure your system is tolerant of duplicate data. This feature is currently only available to those that have been approved for Academic Research access. To learn more about this access level, please visit our section on Academic Research.
#' @param file_name character with name of file. By default, a temporary file is created, tweets are parsed and returned to parent environment, and the temporary file is deleted.
#' @param verbose logical, indicating whether or not to include output processing/retrieval messages.
#' @param parse logical,indicating whether to return parsed data. By default, parse = FALSE, this function does the parsing for you. However, for larger streams, or for automated scripts designed to continuously collect data, this should be set to false as the parsing process can eat up processing resources and time.

#' @return a data frame
#' @export
#'
#' @examples
#' \dontrun{
#' filtered_stream(Token = your_token, timeout = 60, backfill = 0,
#'                file_name = "test_stream_2.json", verbose = T,
#'                parse = F)
#'
#' tdf <- filtered_stream(Token = your_token, timeout = 60, backfill = 0,
#'                       verbose = T, parse = T)
#' }
#'
#' @import httr httpuv RCurl ROAuth data.table readr
#' @importFrom stats na.omit
#' @importFrom lubridate as_datetime
#' @importFrom jsonlite fromJSON toJSON
#' @importFrom dplyr bind_rows
#' @importFrom progress progress_bar
#' @importFrom curl curl
#' @importFrom rlang has_name
#' @importFrom withr defer
##################################################################################################
# Get Tweets from the filtered stream endpoint
##################################################################################################
filtered_stream <- function(token = "",
                           timeout = 0,
                           backfill = 0,
                           file_name = NULL,
                           verbose = T,
                           parse = F){

  # Check if Bearer Token is set:
  if(is.na(token) | nchar(token) < 90){
    stop("Please add the Bearer Token of your projects dashboard!\n")
  }


  # Set filename
  if (is.null(file_name)) {
    file_name <- tempfile(pattern = "filtered_stream_tweets", fileext = ".json")
    inform(paste0("Writing to '", file_name, "'"))
  }
  output <- file(file_name)


  #Add token to header
  headers <- c(`Authorization` = sprintf('Bearer %s', token))
  header_for_curl <- curl::new_handle()
  curl::handle_setheaders(header_for_curl, "Authorization" = sprintf('Bearer %s', token))

  #Set parameters
  tweet_fields <- "attachments,author_id,context_annotations,conversation_id,created_at,entities,geo,id,in_reply_to_user_id,lang,possibly_sensitive,public_metrics,referenced_tweets,reply_settings,source,text,withheld"
  user_fields <- "created_at,description,entities,id,location,name,pinned_tweet_id,profile_image_url,protected,public_metrics,url,username,verified,withheld"
  expansions <- "attachments.poll_ids,attachments.media_keys,author_id,entities.mentions.username,geo.place_id,in_reply_to_user_id,referenced_tweets.id,referenced_tweets.id.author_id"
  place_fields <- "contained_within,country,country_code,full_name,geo,id,name,place_type"
  media_fields <- "duration_ms,height,media_key,preview_image_url,type,url,width,public_metrics"
  backfill_minutes <- backfill
  if(backfill_minutes == 0){
    backfill_minutes <- NULL
  }

  params = list(
    `tweet.fields` = tweet_fields,
    `user.fields` = user_fields,
    `expansions` = expansions,
    `place.fields`= place_fields,
    `media.fields` = media_fields,
    `backfill_minutes` = backfill_minutes)


  #Build API call
  url <- "https://api.twitter.com/2/tweets/search/stream"
  url_fin <- httr::modify_url(url,
                              query = params)

  stream <- curl::curl(url = url_fin, handle = header_for_curl)

  # Stream data
  quiet_interrupt(download_from_stream(stream, output,
                                       timeout = timeout,
                                       verbose = verbose
  ))

  if (parse) {
    df <- parse_stream(file(file_name), filtered = T)
  } else {
    invisible(NULL)
  }

}

##################################################################################################
# Error Handler
##################################################################################################
quiet_interrupt <- function(code) {
  tryCatch(code, interrupt = function(e) NULL)
}
