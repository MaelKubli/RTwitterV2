# ' @title: RTweetV2 Function collecting tweets through teh academic trac search track

#' This function allows you to collect tweets with the academic track with a query using the rulse put in place by twitter (see: https://developer.twitter.com/en/docs/twitter-api/v1/rules-and-filtering/search-operators)
#' @param token string of the bearer token from your personal twitter API access
#' @param user_id string of the users id from the user of which you would like to get the timeline from
#' @param tweet_fields string which defaults to ALL (no other argument accepted at the moment)
#' @param user_fields string which defaults to ALL (no other argument accepted at the moment)
#' @param since_id character containing the lower bound status id to start looking for tweets (default is NA)
#' @param until_id character containing the upper bound status id to start looking for tweets (default is NA)
#' @param start_time character containing the start time for the search (defaults to NA; style is "yyyy-mm-ddThh:mm:ssZ")
#' @param end_time character containing the end time for the search (defaults to NA; style is "yyyy-mm-ddThh:mm:ssZ")
#' @param api_wait integer specifying how long the function should wait for the API to answer (defaults to 12 seconds)
#' @param expansions string which defaults to ALL (no other argument accepted at the moment)
#' @param place_fields string which defaults to ALL (no other argument accepted at the moment)
#' @param media_fields string which defaults to ALL (no other argument accepted at the moment)
#' @param poll_fields string which defaults to NONE (no other argument accepted at the moment)
#' @param n integer specifying the maximum number of tweets the function should retun (defaults to NA)
#' @param JSON boolean which defaults to FALSE
#' @param storage_path character string specifying the path and file name to write the json file to

#' @return a data frame
#' @export
#'
#' @examples
#' \dontrun{
#' users <- full_archive_search(token=bearer_token, query = "Twitter OR #TwitterAPI", start_time = "2020-01-01T00:00:01Z" end_time = "2020-01-02T00:00:01Z", n = 1000)
#' }

#' @import httr
#' @import httpuv
#' @import RCurl
#' @import ROAuth
#' @import jsonlite
#' @import data.table
#' @import purrr
#' @import lubridate
#' @import readr

##################################################################################################
# Get Timelines of Users by ID (only ID Works at the moment)
##################################################################################################
full_archive_search <- function(token = NA, search_query = NA, tweet_fields = "ALL", user_fields = "ALL",
                                since_id = NA, until_id = NA, start_time = NA, end_time = NA, api_wait = 12,
                                expansions = "ALL", place_fields = "ALL", media_fields = "ALL", poll_fields = "NONE",
                                n = NA, JSON = FALSE, storage_path = "archive_searched_tweets.json"){


  # Check if Bearer Token is set:
  if(is.na(token) | nchar(token) != 112){
    stop("Please add the Bearer Token of your projects dashboard!\n")
  }
  # Check if at least one User ID is set:
  if(is.na(search_query) | !is.character(search_query)){
    stop("Please add at least one Search Term to the Search Query!\n")
  }

  if(is.na(n) & is.na(since_id) & is.na(start_time)){
    stop("Pleaes add at least one of the following criterias:\n")
  }

  # mkdir if files should be stored as JSON
  if(JSON == TRUE){
  }

  params <- NULL

  params = list(
    `max_results` = n,
    `start_time` = start_time,
    `end_time` = end_time,
    `since_id` = since_id,
    `until_id` = until_id,
    `query` = search_query,
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


  # define scope of search (max_results / since_id / start_time)
  if(is.na(n)){
    params$max_results <- NULL
    n_max <- 1000000
  } else {
    params$max_results <- 500
    n_max <- n
  }

  # define scope of search (max_results / since_id / start_time)
  if(is.na(since_id)){
    params$since_id <- NULL
  }

  # define scope of search (max_results / since_id / start_time)
  if(is.na(until_id)){
    params$until_id <- NULL
  }

  # define scope of search (max_results / since_id / start_time)
  if(is.na(start_time)){
    params$start_time <- NULL
    start_time_check <-lubridate::as_datetime(Sys.time()) - 10000
  } else {
    start_time_check <- start_time
  }

  # define scope of search (max_results / since_id / start_time)
  if(is.na(end_time)){
    params$end_time <- NULL
    end_time_check <-lubridate::as_datetime(Sys.time())
  } else {
    end_time_check <- end_time
  }


  # setup header for authentication
  headers <- c(`Authorization` = sprintf('Bearer %s', token))

  # loop through API output
  if(is.na(n) | n > 500){

    # basic values for query
    get_results <- TRUE
    count_results <- 500
    results_count <- 0
    params$max_results <- 500
    date_check_l <- start_time_check
    date_check_u <- end_time_check
    counter <- 1
    pg_token <- ""
    n_check <- n_max

    while(get_results == TRUE){
      # Add Pagination Token
      if(counter != 1){
        params$next_token <- pg_token

        if(rate_limit_remaining < 1){
          cat(paste0("Rate Limit Reached! Function will wait for ", difftime(rate_limit_reset, Sys.time(), units='mins'),"minutes.\n"))
          wait_time <- difftime(rate_limit_reset, Sys.time(), units='secs') + 30
          Sys.sleep(wait_time)
        }

      }
      # If Search is exhausted!
      if(pg_token == "no_next_token"){
        get_results = FALSE
        counter <- 2
      } else if (lubridate::as_datetime(date_check_l) > lubridate::as_datetime(date_check_u)){
        get_results = FALSE
        counter <- 2
      } else if (results_count >= n_check) {
        get_results = FALSE
        counter <- 2
      } else {
        response <- httr::GET(url = 'https://api.twitter.com/2/tweets/search/all', httr::add_headers(.headers=headers), query = params, timeout(api_wait))
        Sys.sleep(.1)
        if(response[["status_code"]] == 503){
          Sys.sleep(1)
          response <- httr::GET(url = 'https://api.twitter.com/2/tweets/search/all', httr::add_headers(.headers=headers), query = params, timeout(api_wait))
          ww <- 1
          while(response[["status_code"]] != 200){
            Sys.sleep(ww)
            ww <- ww + 1
            cat("Something went wrong!\n")
            cat(paste0(content(response)$errors[[1]]$message,"\nError: ",content(response)$status),"\n")
            response <- httr::GET(url = 'https://api.twitter.com/2/tweets/search/all', httr::add_headers(.headers=headers), query = params, timeout(api_wait))
          }
        } else if (response[["status_code"]] != 200){
          cat("Something went wrong!\n")
          stop(paste0(content(response)$errors[[1]]$message,"\nError: ",content(response)$status),"\n")
        } else {

        }

        rate_limit_remaining <- response[["headers"]][["x-rate-limit-remaining"]]
        rate_limit_reset <- as.POSIXct(as.numeric(response[["headers"]][["x-rate-limit-reset"]]), origin = "1970-01-01")

        if(JSON == FALSE){
          # return Data
          results_list <- jsonlite::fromJSON(httr::content(response, "text"), simplifyDataFrame =  F)
          count_results <- results_list[["meta"]][["result_count"]]
          ret <- data_parser_search_full(results_data = results_list)
          data_twitter <- ret[[1]]
          pg_token <- ret[[2]]

          #bind data
          if(counter == 1){
            data <- data_twitter
            counter <- 2

            results_count <- nrow(data)
            date_check_l <- max(data$created_at)
          } else {
            data <- dplyr::bind_rows(data,data_twitter)
            results_count <- nrow(data)
            date_check_l <- max(data$created_at)
          }


        } else {
          data_twitter <- jsonlite::fromJSON(httr::content(response, "text"))

          if(length(data_twitter[["meta"]]) == 4){
            pg_token <- data_twitter[["meta"]][["next_token"]]
          } else if (length(data_twitter[["meta"]]) == 5){
            pg_token <- data_twitter[["meta"]][["next_token"]]
          } else {
            pg_token <- "no_next_token"
          }

          ret <- .data_json(data_twitter = data_twitter)
          pg_token <- ret[[2]]

          #bind data
          if(counter == 1){
            data <- data_twitter
            counter <- 2

            results_count <- nrow(data)
            date_check <- max(data$created_at)
          } else {
            data <- dplyr::bind_rows(data,data_twitter)
            results_count <- nrow(data)
            date_check <- max(data$created_at)
          }


          if(!file.exists(storage_path) & pg_token != "no_next_token"){
            data_j <- jsonlite::toJSON(ret[[1]], pretty = T)
            data_j <- gsub('.{0,2}$', ',', data_j)
            write_file(data_j, file = storage_path, append = F)
          } else if(file.exists(storage_path) & pg_token != "no_next_token") {
            data_j <- jsonlite::toJSON(ret[[1]], pretty = T)
            data_j <- gsub('.{0,2}$', ',', data_j)
            data_j <- gsub('^.{0,2}', '', data_j)
            write_file(data_j, file = storage_path, append = T)
          } else if(!file.exists(storage_path) & pg_toke == "no_next_token") {
            data_j <- jsonlite::toJSON(ret[[1]], pretty = T)
            write_file(data_j, file = storage_path, append = F)
          } else if(file.exists(storage_path) & pg_token == "no_next_token") {
            data_j <- jsonlite::toJSON(ret[[1]], pretty = T)
            data_j <- gsub('^.{0,2}', '', data_j)
            write_file(data_j, file = storage_path, append = T)
          } else {

          }

        }
      }
    }
  } else {
    response <- httr::GET(url = 'https://api.twitter.com/2/tweets/search/all', httr::add_headers(.headers=headers), query = params, timeout(10))

    rate_limit<- response[["headers"]][["x-rate-limit-limit"]]
    rate_limit_remaining <- response[["headers"]][["x-rate-limit-remaining"]]
    rate_limit_reset <- as.POSIXct(as.numeric(response[["headers"]][["x-rate-limit-reset"]]), origin = "1970-01-01")

    if(JSON == FALSE){
      # return Data
      results_list <- content(response, "text", encoding = "UTF-8")
      results_list <- jsonlite::fromJSON(results_list)
      ret <- data_parser_search_full(results_data = results_list)
      data <- ret[[1]]
    } else {
      data <- jsonlite::fromJSON(httr::content(response, "text"))
      ret <- data_json(data_twitter = data)

      data_j <- jsonlite::toJSON(ret[[1]], pretty = T)
      write_file(data_j, file = storage_path, append = F)

      data <- ret[[1]]

    }
  }
  return(data)
}


