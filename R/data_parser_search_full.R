# ' @title: RTweetV2 Function data parser full archive search

#' This function is a sub function parsing the data returned from the API
#' @param results_data list with results from the API call

#' @return data frame
#' @export
#'

#' @import httr httpuv RCurl ROAuth data.table readr
#' @importFrom purrr map

##################################################################################################
# Parse full_archive_search
##################################################################################################
data_parser_search_full <- function(results_data){

  data_list <- results_data$data
  if(is.list(data_list) == F){data_list <- list(data_list)}

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
    if(ncol(tmp)==0){

    } else {
      tmp <- setnames(tmp, paste0('attachments.media_keys_', 1:ncol(tmp)))[]
      dt <- cbind(dt,tmp)
    }
  }

  h <- grep("geo\\.place_id", names(dt), value = T)
  if(length(h) != 0){
    tmp <- setDT(dt)[, data.table::tstrsplit(dt$geo.place_id,
                                             ',', perl=TRUE)]

    if(ncol(tmp)==0){

    } else {
      tmp <- setnames(tmp, paste0('geo.place_id_', 1:ncol(tmp)))[]
      dt <- cbind(dt,tmp)
    }
  }
  ##################################################################
  # ---- includes part ---- #
  ##################################################################
  includes_list <- results_data$includes
  ##################################################################
  # ----- users data ----- #
  ##################################################################
  ## users fields
  user_list <- includes_list$users
  du <- lapply(user_list,flattenlist)
  du <- purrr::map(du, as.data.table)
  du <- data.table::rbindlist(du, fill = TRUE)
  du <- as.data.frame(du)
  ## Remove cols we have no use for
  h <- grep("\\.start$|\\.end$", names(du), value = T)
  du <- du[ , !names(du) %in% h]

  # Transform User Data
  du <- internal_parser_user(du)

  # ----- split user of time-line and users which are retweeted and quoted -----#
  ## Select Data form Users tweets are quoted and re-tweeted
  dqr <- du[!du$user_id %in% unique(dt$user_id), ]
  ## Select Data form User of Time-line only
  du <- du[du$user_id %in% unique(dt$user_id), ]
  ## Distinct User ID's
  du <- du[!duplicated(du$user_id), ]
  ## Add User Fields to Tweets
  dt <- data.table::merge.data.table(dt,du, by = c("user_id"))
  ##################################################################
  # ----- media add-on for dt ----- #
  ##################################################################
  media_list <- includes_list$media
  if(is.null(media_list)==FALSE){
    dm <- lapply(media_list,flattenlist)
    dm <- purrr::map(dm, as.data.table)
    dm <- data.table::rbindlist(dm, fill = TRUE)

    dt <- internal_parser_media(dm = dm, dt = dt)
  }
  ##################################################################
  # ----- places add-on for dt ----- #
  ##################################################################
  places_list <- includes_list$places
  if(is.null(places_list)==FALSE){
    for(i in 1:length(places_list)){
      places_list[[i]]$geo$bbox <- gsub("\\,\\s","\\,", toString(na.omit(places_list[[i]]$geo$bbox)))
    }
    dp <- lapply(places_list,flattenlist)
    dp <- purrr::map(dp, as.data.table)
    dp <- data.table::rbindlist(dp, fill = TRUE)

    dt <- internal_parser_places(dp,dt)
  }
  ##################################################################
  # ----- quoted and retweeted tweets data ----- #
  ##################################################################
  tweet_list <- includes_list$tweets
  if(is.null(tweet_list)) {
    location <- TRUE
  } else {
    location <- FALSE
    for(i in 1:length(tweet_list)){
      tweet_list[[i]]$attachments$media_keys <- gsub("\\,\\s","\\,", toString(na.omit(tweet_list[[i]]$attachments$media_keys)))
    }
    drrqt <- lapply(tweet_list,flattenlist)
    drrqt <- purrr::map(drrqt, as.data.table)
    drrqt <- data.table::rbindlist(drrqt, fill = TRUE)
    drrqt <- as.data.frame(drrqt)

    drrqt <- tweets_transformer(drrqt)

    dt <- internal_parser_rt_qt(drrqt = drrqt, dqr = dqr, dt = dt)
  }

  ##################################################################
  # ----- places data for tweet ----- #
  ##################################################################
  #### Future Update ....

  ##################################################################
  # ----- remove key columns used to add from different lists from API----- #
  ##################################################################
  h <- grep("\\.place_id", names(dt), value = T)
  if(length(h) != 0){
    dt <- as.data.frame(dt)
    dt <- dt[ , !names(dt) %in% h]
  }

  h <- unique(dt$referenced_tweets_type)

  if(location == T){
    dt$is_retweet <- 0
    dt$is_quote <- 0
  } else {
    dt$is_retweet <- ifelse(grepl("retweeted", dt$referenced_tweets_type) == T, 1, 0 )
    dt$is_quote <- ifelse(grepl("quoted", dt$referenced_tweets_type) == T, 1, 0 )
  }

  if(length(h) != 0){
    dt$referenced_tweets_id <- NULL
    dt$referenced_tweets_type <- NULL
  }

  dt <- as.data.frame(dt)
  dt <- dt[ , order(names(dt))]
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
