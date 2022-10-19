# ' @title: RTweetV2 Function collecting tweets

#' This function allows you to collect tweets with there status_id (max 100 tweet ids per call)
#' @param token string of the bearer token from your personal twitter API access
#' @param tweet_ids string representing the query to search tweets with
#' @param tweet_fields string which defaults to ALL (no other argument accepted at the moment)
#' @param user_fields string which defaults to ALL (no other argument accepted at the moment)
#' @param api_wait integer specifying how long the function should wait for the API to answer (defaults to 12 seconds)
#' @param expansions string which defaults to ALL (no other argument accepted at the moment)
#' @param place_fields string which defaults to ALL (no other argument accepted at the moment)
#' @param media_fields string which defaults to ALL (no other argument accepted at the moment)
#' @param poll_fields string which defaults to NONE (no other argument accepted at the moment)
#' @param JSON boolean which defaults to FALSE
#' @param storage_path character string specifying the path and file name to write the json file to
#' @param n_try integer specifying number of retries in case of 503 error

#' @return a data frame
#' @export
#'
#' @examples
#' \dontrun{
#' users <- get_tweets(token=bearer_token, tweet_ids = "123456789458,45678988975744")
#' }

#' @import httr httpuv RCurl ROAuth data.table readr
#' @importFrom stats na.omit
#' @importFrom lubridate as_datetime
#' @importFrom jsonlite fromJSON toJSON
#' @importFrom dplyr bind_rows
##################################################################################################
# Get Timelines of Users by ID (only ID Works at the moment)
##################################################################################################
get_tweets <- function(token = NA, tweet_ids = NA, tweet_fields = "ALL", user_fields = "ALL",
                                api_wait = 12, expansions = "ALL", place_fields = "ALL",
                                media_fields = "ALL", poll_fields = "NONE",
                                JSON = FALSE, storage_path = "get_tweets_by_ids.json", n_try = 10){


  # Check if Bearer Token is set:
  if(is.na(token) | nchar(token) < 90){
    stop("Please add the Bearer Token of your projects dashboard!\n")
  }
  # Check if at least one User ID is set:
  if(is.na(tweet_ids) | !is.character(tweet_ids)){
    stop("Please add at least one Tweet ID to the Query!\n")
  }

  # mkdir if files should be stored as JSON
  if(JSON == TRUE){
  }

  params <- NULL

  params = list(
    `tweet.fields` = tweet_fields,
    `user.fields` = user_fields,
    `expansions` = expansions,
    `place.fields`= place_fields,
    `media.fields` = media_fields,
    `poll.fields` = poll_fields)

  # Set which fields to return from Tweet
  if(params$tweet.fields == "ALL"){
    params$tweet.fields <- "attachments,author_id,context_annotations,conversation_id,created_at,entities,geo,id,in_reply_to_user_id,lang,possibly_sensitive,public_metrics,referenced_tweets,reply_settings,source,text,withheld"
  } else if (params$tweet.fields == "NONE"){
    params$tweet.fields <- NULL
  } else {
    # Keep Current Query for tweet fields
  }

  # Set which fields to return from user
  if(params$user.fields == "ALL"){
    params$user.fields <- "created_at,description,entities,id,location,name,pinned_tweet_id,profile_image_url,protected,public_metrics,url,username,verified,withheld"
  } else if(params$user.fields == "NONE"){
    params$user.fields <- NULL
  } else {
    # Keep Current Query for tweet fields
  }

  # Set which fields to return from expansions
  if(params$expansions == "ALL"){
    params$expansions <- "attachments.poll_ids,attachments.media_keys,author_id,entities.mentions.username,geo.place_id,in_reply_to_user_id,referenced_tweets.id,referenced_tweets.id.author_id"
  } else if(params$expansions == "NONE"){
    params$expansions <- NULL
  } else {
    # Keep Current Query for tweet fields
  }

  # Set which fields to return from place
  if(params$place.fields == "ALL"){
    params$place.fields <- "contained_within,country,country_code,full_name,geo,id,name,place_type"
  } else if(params$place.fields == "NONE"){
    params$place.fields <- NULL
  } else {
    # Keep Current Query for tweet fields
  }

  # Set which fields to return from polls
  if(params$poll.fields == "ALL"){
    params$poll.fields <- "duration_minutes,end_datetime,id,options,voting_status"
  } else if(params$poll.fields == "NONE"){
    params$poll.fields <- NULL
  } else {
    # Keep Current Query for tweet fields
  }

  # Set which fields to return from media
  if(params$media.fields == "ALL"){
    params$media.fields <- "duration_ms,height,media_key,preview_image_url,type,url,width,public_metrics"
  } else if(params$media.fields == "NONE"){
    params$media.fields <- NULL
  } else {
    # Keep Current Query for tweet fields
  }



  # setup header for authentication
  headers <- c(`Authorization` = sprintf('Bearer %s', token))

  #
  response <- httr::GET(url = paste0('https://api.twitter.com/2/tweets?ids=',tweet_ids), httr::add_headers(.headers=headers), query = params, timeout(api_wait))
  Sys.sleep(.1)
  ww <- 1
  if(response[["status_code"]] == 503){
    Sys.sleep(1)
    response <- httr::GET(url = paste0('https://api.twitter.com/2/tweets?ids=',tweet_ids), httr::add_headers(.headers=headers), query = params, timeout(api_wait))
    while(response[["status_code"]] != 200 & ww <= n_try){
      Sys.sleep(ww)
      ww <- ww + 1
      cat("Something went wrong!\n")
      cat(paste0(content(response)$errors[[1]]$message,"\nError: ",content(response)$status),"\n")
      response <- httr::GET(url = paste0('https://api.twitter.com/2/tweets?ids=',tweet_ids), httr::add_headers(.headers=headers), query = params, timeout(api_wait))
    }
  } else if (response[["status_code"]] != 200){
    cat("Something went wrong!\n")
    stop(paste0(content(response)$errors[[1]]$message,"\nError: ",content(response)$status),"\n")
  } else {

  }

  rate_limit<- response[["headers"]][["x-rate-limit-limit"]]
  rate_limit_remaining <- response[["headers"]][["x-rate-limit-remaining"]]
  rate_limit_reset <- as.POSIXct(as.numeric(response[["headers"]][["x-rate-limit-reset"]]), origin = "1970-01-01")

  if(JSON == FALSE){
   # return Data
    results_list <- jsonlite::fromJSON(httr::content(response, "text"), simplifyDataFrame =  F)
    ret <- data_parser_search_full(results_data = results_list)
    data <- ret[[1]]
  } else {
    data <- jsonlite::fromJSON(httr::content(response, "text"))
    ret <- data_json(data_twitter = data)

    data_j <- jsonlite::toJSON(ret[[1]], pretty = T)
    write_file(data_j, file = storage_path, append = F)

    data <- ret[[1]]

  }

  return(data)
}
