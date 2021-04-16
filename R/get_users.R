# ' @title: RTweetV2 Function getting Users Information from Twitters v2 API
##################################################################################################
# Twitter API V2 Endpoint Functions
##################################################################################################
# Description:
# This Script contains the functions necessary to query the new twitter API v2 endpoints.
# See https://developer.twitter.com/en/docs for more info about the API
##################################################################################################
# Content
##################################################################################################
# Get Users by ID and User_Name
##################################################################################################
get_user_v2 <- function(token = NA, user_ids = NA, user_names = NA, user_fields = "ALL", tweet_fields = "author_id", expansions = "ALL", JSON = FALSE){
  # required packages:
  suppressPackageStartupMessages(require(httr))
  suppressPackageStartupMessages(require(rjson))
  suppressPackageStartupMessages(require(jsonlite))
  suppressPackageStartupMessages(require(data.table))
  suppressPackageStartupMessages(require(httpuv))
  suppressPackageStartupMessages(require(RCurl))
  suppressPackageStartupMessages(require(ROAuth))
  suppressPackageStartupMessages(require(purrr))
  suppressPackageStartupMessages(require(lubridate))
  suppressPackageStartupMessages(require(readr))

  # Check if Bearer Token is set:
  if(is.na(token) | nchar(token) != 112){
    stop("Please add the Bearer Token of your projects dashboard!\nget_timelines_v2(token = 'token')\n")
  }
  # Check if at least one User ID is set:
  if(is.na(user_ids) & is.na(user_names)){
    stop("Please add at least one Search Term to the Search Query!\n")
  }
  if(is.na(user_ids) == T & is.na(user_names) == T){
    stop("Please add at least one user_ids or user_names to the Search Query!\n")
  }

  params = list(
    `usernames` = user_names,
    `ids` = user_ids,
    `tweet.fields` = tweet_fields,
    `user.fields` = user_fields,
    `expansions` = expansions)

  # Set which fields to return from Tweet
  if(params$tweet.fields == "ALL"){
    params$tweet.fields <- "attachments,author_id,context_annotations,conversation_id,created_at,entities,geo,id,in_reply_to_user_id,lang,non_public_metrics,public_metrics,organic_metrics,promoted_metrics,possibly_sensitive,referenced_tweets,reply_settings,source,text,withheld"
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
    params$expansions <- "pinned_tweet_id"
  } else if(params$expansions == "NONE"){
    params$expansions <- NULL
  } else {
    # Keep Current Query for tweet fields
  }

  # setup header for authentification
  headers <- c(`Authorization` = sprintf('Bearer %s', token))
  if(is.na(user_ids) == T){
    if(grepl(",",user_names) == T){
      params$ids <- NULL
      params$username <- NULL
      response <- httr::GET(url = paste0("https://api.twitter.com/2/users/by"), httr::add_headers(.headers=headers),query = params)
    } else {
      params$ids <- NULL
      params$usernames <- NULL
      response <- httr::GET(url = paste0("https://api.twitter.com/2/users/by/username/",user_names), httr::add_headers(.headers=headers),query = params)
    }

  } else {
    if(grepl(",",user_ids) == T){
      params$usernames <- NULL
      response <- httr::GET(url = paste0("https://api.twitter.com/2/users"), httr::add_headers(.headers=headers),query = params)
    } else {
      params$usernames <- NULL
      response <- httr::GET(url = paste0("https://api.twitter.com/2/users"), httr::add_headers(.headers=headers),query = params)
    }

  }

  #print(response)

  if(response[["status_code"]] == 200){
    cat("Status 200: Everything went fine!\n")
  } else if(response[["status_code"]] == 400) {
    cat("Something went wrong!\n")
    cat(paste0(content(response)$errors[[1]]$message,"\n"))
  } else if (response[["status_code"]] == 404){
    cat("Something went wrong!\n")
    cat(paste0(content(response)$errors[[1]]$message,"\n"))
  } else {

  }

  results_list <- content(response)
  results_data <- results_list[["data"]]

  data <- data_parser_users(results_data)

  if(JSON == TRUE){
    return()
  } else {
    return(data)
  }
}

