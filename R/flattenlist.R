# ' @title: RTweetV2 Function flattenlist
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
# flatten list of lists where lists are nested within and of unequal length!
flattenlist <- function(x){
  morelists <- sapply(x, function(xprime) class(xprime)[1]=="list")
  out <- c(x[!morelists], unlist(x[morelists], recursive=FALSE))
  if(sum(morelists)){
    Recall(out)
  }else{
    return(out)
  }
}