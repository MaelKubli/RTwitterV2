#!/usr/bin/Rscript
##################################################################################################
# Twitter API V2 Endpoint Functions
##################################################################################################
# Description:
# This Script contains the functions necessary to query the new twitter API v2 endpoints.
# See https://developer.twitter.com/en/docs for more info about the API
##################################################################################################
# Content
##################################################################################################
# Dependencies
##################################################################################################
library(httr)
library(httpuv)
library(RCurl)
library(ROAuth)
library(jsonlite)
library(data.table)
library(purrr)
library(lubridate)
library(readr)
##################################################################################################
# Get Timelines of Users by ID (only ID Works at the moment)
##################################################################################################
get_timelines_v2 <- function(token = NA, user_id = "783214", tweet_fields = "ALL", user_fields = "ALL", 
                             expansions = "ALL", place_fields = "ALL", poll_fields = "NONE", n = 100, JSON = FALSE)
{
  # required packages:
  suppressPackageStartupMessages(require(httr))
  suppressPackageStartupMessages(require(rjson))
  suppressPackageStartupMessages(require(jsonlite))
  suppressPackageStartupMessages(require(data.table))
  
  # Check if Bearer Token is set:
  if(is.na(token) | nchar(token) != 112){
    stop("Please add the Bearer Token of your projects dashboard!\nget_timelines_v2(token = 'token')\n")
  }
  # Check if at least one User ID is set:
  if(is.na(user_id) | !is.character(user_id)){
    stop("Please add at least one Search Term to the Search Query!\n")
  }
  # Check if n is within range
  if(n > 3200){
    stop("Do not query more than 3200 Tweets per Timeline as it is not supported!\n")
  }
  if(grepl('.\\,', as.character(user_id)) == T){
    stop("Currently this function only works with one ID at a time!\n")
  }
  
  params = list(
    `max_results` = n,
    `tweet.fields` = tweet_fields,
    `user.fields` = user_fields,
    `expansions` = expansions,
    `place.fields`= place_fields,
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
    params$expansions <- "author_id,referenced_tweets.id,referenced_tweets.id.author_id,entities.mentions.username,attachments.poll_ids,attachments.media_keys,in_reply_to_user_id,geo.place_id"
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
  
  # setup header for authentification 
  headers <- c(`Authorization` = sprintf('Bearer %s', Bearer_Token))

  
  # pagination or not 
  if(n < 101){
    
    # setup parameter for max_results
    params$max_results <- 100    
    
    response <- httr::GET(url = paste0('https://api.twitter.com/2/users/',user_id,'/tweets'), httr::add_headers(.headers=headers),query = params)
    #print(response)
    
    # return Data
    results_list <- fromJSON(content(response, "text", encoding = "UTF-8"))
    ret <- data_parser_timeline(results_list)
    data <- ret[[1]]
  } else {
    rate_limit_remaining <- 100
    n_pg <- ceiling(n / 100) 
    modulo <- n %% 100
    for(i in 1:n_pg){
      if(i == 1){
        #setup param for max_results
        params$max_results <- 100
        pg_token <- "Noe"
      } else if(i == n_pg){
        if(modulo == 0){
          modulo_n <- 100
        } else {
          modulo_n <- modulo
        }
        #setup param for max_results
        params$max_results <- modulo_n
        params$pagination_token <-  pg_token
      } else {
        #setup param for max_results
        params$max_results <- 100
        params$pagination_token <-  pg_token
      }
      if(pg_token =="no_next_token"){
        
      } else {
        
        if(rate_limit_remaining < 1){
          cat(paste0("Rate Limit Reached! Function will wait for ", difftime(rate_limit_reset, Sys.time(), units='mins'),"minutes.\n"))
          wait_time <- difftime(rate_limit_reset, Sys.time(), units='secs') + 30
          Sys.sleep(wait_time)
        }
        
        response <- httr::GET(url = paste0('https://api.twitter.com/2/users/',user_id,'/tweets'), httr::add_headers(.headers=headers),query = params)
        #print(response)
        
        if(response[["status_code"]] == 200){
          #cat("Status 200: Everything went fine!\n")
        } else if(response[["status_code"]] == 400) {
          cat("Something went wrong!\n")
          cat(paste0(content(response)$errors[[1]]$message,"\n")) 
        } else if (response[["status_code"]] == 404){
          cat("Something went wrong!\n")
          cat(paste0(content(response)$errors[[1]]$message,"\n")) 
        } else {
          
        }
        
        results_list <- jsonlite::fromJSON(httr::content(response, "text"), simplifyDataFrame =  F)
        
        rate_limit<- response[["headers"]][["x-rate-limit-limit"]]
        rate_limit_remaining <- response[["headers"]][["x-rate-limit-remaining"]]
        rate_limit_reset <- as.POSIXct(as.numeric(response[["headers"]][["x-rate-limit-reset"]]), origin = "1970-01-01")
        
        ret <- data_parser_timeline(results_list)
        pg_token <- ret[[2]]
        ret <- ret[[1]]
        
        if(i == 1){
          data <- ret
        } else {
          data <- dplyr::bind_rows(data,ret)
        }
      }
     
    }
  }
  return(data)
}

