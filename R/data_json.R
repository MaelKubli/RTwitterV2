# ' @title: RTweetV2 Function handling data as JSON

#' This function is a sub function parsing the data returned from the API
#' @param data_twitter list with results from the API call

#' @return list writable as a JSON file
#' @export
#'

#' @import httr httpuv RCurl ROAuth data.table readr

##################################################################################################
# Helper Functions
##################################################################################################
##################################################################################################
# Save Data as JSON File (save each iteration and append new iteration to file)
##################################################################################################
# Align with streaming layout for parser
data_json <- function(data_twitter = data){

  #--------------------------------
  # Twitter List
  #--------------------------------
  dt <- data_twitter$data
  colnames(dt)[colnames(dt) == "author_id"] <- "user_id"
  colnames(dt)[colnames(dt) == "id"] <- "status_id"
  #--------------------------------
  # Includes List
  #--------------------------------
  include <- data_twitter$includes

  ## User List
  du <- include$users
  colnames(du)[colnames(du) == "id"] <- "user_id"
  colnames(du)[colnames(du) == "created_at"] <- "account_created_at"
  ## Referenced Tweets List
  dr <- include$tweets
  colnames(dr)[colnames(dr) == "id"] <- "status_id"
  dr <- as.data.table(dr)
  dr <- unique(dr, by = c("status_id"))
  ## Places List
  dp <- include$places
  colnames(dp)[colnames(dp) == "id"] <- "geo.place.id"
  ## Media List
  dm <- include$media
  dm_special <- dm$public_metrics
  dm$public_metrics <- NULL
  dm <- cbind(dm,dm_special)
  colnames(dm) <- paste0("media_",names(dm))

  #--------------------------------
  # Build JSON
  #--------------------------------
  # Add User Data to tweets
  df <- merge.data.frame(dt,du, by.x = "user_id", by.y = "user_id", all.x = T, all.y = F, suffixes = c("_tweet","_user"))
  df <- as.data.table(df)
  # Add Referenced Tweets List
  h <- rbindlist(lapply(df$referenced_tweets, function(x) if(is.null(x)) data.frame(type = NA, id = NA) else x))
  df$bind_retweeted <- ifelse(grepl("retweeted", h$type) == T, h$id, NA)
  df$bind_quoted <- ifelse(grepl("quoted", h$type) == T, h$id, NA)
  df$bind_replied <- ifelse(grepl("replied_to", h$type) == T, h$id, NA)

  df <- merge.data.table(df,dr, by.x = "bind_retweeted", by.y = "status_id", all.x = T, all.y = F, suffixes = c("","_retweet"))
  df <- merge.data.table(df,dr, by.x = "bind_quoted", by.y = "status_id", all.x = T, all.y = F, suffixes = c("","_quoted"))
  df <- merge.data.table(df,dr, by.x = "bind_replied", by.y = "status_id", all.x = T, all.y = F, suffixes = c("","_replied_to"))

  # Add Places
  dp$geo_feature <- dp$geo[1]
  dp$geo_bbox <- paste0(unlist(dp$geo[2]),collapse = ",")
  dp$geo <- NULL

  colnames(df)[colnames(df) == "geo.place_id"] <- "geo.place_id_real"

  df <- merge.data.frame(df,dp, by.x = "geo.place_id_real", by.y = "geo.place.id", all.x = T, all.y = F)
  df$geo.place_id_real <- NULL


  # Add Media (Way to complex but I found no easier solution)
  dm_i <- data.frame(matrix(nrow = nrow(df), ncol = ncol(dm)))
  colnames(dm_i) <- names(dm)
  for(i in 1:nrow(df)){
    keys <- df$attachments.media_keys[[i]]

    if(!is.null(keys) == T){
     if(length(keys) == 1){
       tmp_j <- subset(dm, `media_media_key` == keys[1])
       if(nrow(tmp_j) == 0){
         tmp_j <- data.frame(matrix(nrow = 1, ncol = 8))
         colnames(tmp_j) <- names(dm)
       }
       dm_i[i,] <- tmp_j
     } else {
       for(j in 1:length(keys)){
         tmp_j <- subset(dm, `media_media_key` == keys[j])
         if(nrow(tmp_j) == 0){
           tmp_j <- data.frame(matrix(nrow = 1, ncol = 8))
           colnames(tmp_j) <- names(dm)
         }
         if(j == 1){
          tmp <- tmp_j
         } else if (j < length(keys)) {
          tmp <- paste0(tmp, ", ",tmp_j)
         } else {
          tmp <- paste0(tmp, ", ",tmp_j)
          if(grepl("NA,\\s|NA$|,\\sNA", tmp[1]) == TRUE){
            tmp <- gsub("NA,\\s|NA$|,\\sNA", "", tmp)
            tmp <- gsub("", "NA", tmp)
          }
          dm_i[i,] <- tmp
         }

       }
     }
    }
  }

  df$attachments.media_keys <- sapply(df$attachments.media_keys, function(x){paste0(x, collapse = ", ")})
  df <- merge.data.frame(df,dm_i, by.x = "attachments.media_keys", by.y = "media_media_key", all.x = T, all.y = F)
  df$bind_quoted <- NULL
  df$bind_replied <- NULL
  df$bind_retweeted <- NULL
  df$attachments.media_keys <- NULL
  df$attachments.media_keys_quoted <- NULL
  df$attachments.media_keys_replied_to <- NULL
  df$attachments.media_keys_retweet <- NULL

  #--------------------------------
  # Next Token
  #--------------------------------
  next_token <- data_twitter$meta

  if(length(next_token) == 4){
    next_token <- next_token$next_token
  } else if (length(next_token) == 5){
    next_token <- next_token$next_token
  } else {
    next_token <- "no_next_token"
  }

  data <- list(df,next_token)
  return(data)
}

