# ' @title: RTweetV2 Function parsing timeline data
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
# Parse Get_Timeline Data
##################################################################################################
# --- testing stuff: --- #
#results_data <- results_data[["data"]]
#results_data_object <- results_data[[36]]
#results_next_token <- results_data[["meta"]]
# --- testing stuff end --- #

data_parser_timeline <- function(results_data){

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

  ##################################################################
  # ---- data part ---- #
  ##################################################################
  data_list <- results_data$data
  for(i in 1:length(data_list)){
    data_list[[i]]$attachments$media_keys <- gsub("\\,\\s","\\,", toString(na.omit(data_list[[i]]$attachments$media_keys)))
  }
  dt <- lapply(data_list,flattenlist)
  dt <- purrr::map(dt, as.data.table)
  dt <- data.table::rbindlist(dt, fill = TRUE)
  dt <- as.data.frame(dt)

  dt <- tweets_transformer(dt)
  ## get columns names right:
  names(dt)[names(dt)=="id"] <- "status_id"
  names(dt)[names(dt)=="author_id"] <- "user_id"
  colnames_new <- gsub("public_metrics.","",names(dt))
  colnames(dt) <- colnames_new

  # split Keys variables...
  h <- grep("attachments\\.media_keys", names(dt), value = T)
  if(length(h) != 0){
    tmp <- setDT(dt)[, data.table::tstrsplit(dt$attachments.media_keys,
                                             ',', perl=TRUE)]
    tmp <- setnames(tmp, paste0('attachments.media_keys_', 1:ncol(tmp)))[]
    dt <- cbind(dt,tmp)
  }

  h <- grep("geo\\.place_id", names(dt), value = T)
  if(length(h) != 0){
    tmp <- setDT(dt)[, data.table::tstrsplit(dt$geo.place_id,
                                             ',', perl=TRUE)]
    tmp <- setnames(tmp, paste0('geo.place_id_', 1:ncol(tmp)))[]
    dt <- cbind(dt,tmp)
  }

  ##################################################################
  # ----- meta pagination token ----- #
  ##################################################################
  next_token <- results_data$meta

  if(length(next_token) == 4){
    next_token <- next_token$next_token
  } else if (length(next_token) == 5){
    next_token <- next_token$next_token
  } else {
    next_token <- "no_next_token"
  }
  dt <- list(dt,next_token)
  return(dt)
}
