# ' @title: RTweetV2 Function combine lists of columns of tweets
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
require(httr)
require(httpuv)
require(RCurl)
require(ROAuth)
require(jsonlite)
require(data.table)
require(purrr)
require(lubridate)
require(readr)
##################################################################################################
# Helper Functions
##################################################################################################
# combine listed columns to one column
combine_list_columns_tweets <- function(h, dt){
  if(length(h) == 1){
    h_ret <- lapply(dt[, h], function(x){y <- gsub("\\,\\s","\\,", toString(na.omit(x)))
    y <- ifelse(y == "", NA, y)
    return(y)})
  } else {
    h_ret <- apply(dt[, h], 1, function(x){y <- gsub("\\,\\s","\\,", toString(na.omit(x)))
    y <- ifelse(y == "", NA, y)
    return(y)})
  }
}