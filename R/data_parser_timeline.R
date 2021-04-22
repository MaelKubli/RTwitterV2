# ' @title: RTweetV2 Function parsing timeline data

#' This function is a sub function parsing the data returned from the API
#' @param results_data list with results from the API call

#' @return data frame
#' @export
#'
#' @examples
#' \dontrun{
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
# Parse Get_Timeline Data
##################################################################################################
# --- testing stuff: --- #
#results_data <- results_data[["data"]]
#results_data_object <- results_data[[36]]
#results_next_token <- results_data[["meta"]]
# --- testing stuff end --- #

data_parser_timeline <- function(results_data){


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
    old_n <- names(tmp)
    tmp <- setnames(tmp, old = old_n, new = paste0('attachments.media_keys_', 1:ncol(tmp)))[]
    dt <- cbind(dt,tmp)
  }

  h <- grep("geo\\.place_id", names(dt), value = T)
  if(length(h) != 0){
    tmp <- setDT(dt)[, data.table::tstrsplit(dt$geo.place_id,
                                             ',', perl=TRUE)]
    old_n <- names(tmp)
    tmp <- setnames(tmp, old = old_n, new = paste0('geo.place_id_', 1:ncol(tmp)))[]
    dt <- cbind(dt,tmp)
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

  ## entities description_hashtags
  h <- grep("entities\\.description\\.hashtags\\d+\\.tag|entities\\.description\\.hashtags\\.tag", names(du), value = T)
  if(length(h) != 0){
    du$description_hashtags <- combine_list_columns_tweets(h = h, dt = du)
    du$description_hashtags <- as.character(du$description_hashtags)
    du <- du[ , !names(du) %in% h]
  }

  ## entities description_mentions
  h <- grep("entities\\.description\\.mentions\\d+\\.username|entities\\.description\\.mentions\\.username", names(du), value = T)
  if(length(h) != 0){
    du$description_mentions <- combine_list_columns_tweets(h = h, dt = du)
    du$description_mentions <- as.character(du$description_mentions)
    du <- du[ , !names(du) %in% h]
  }

  ## url expanded
  h <- grep("entities\\.url\\.urls\\d+\\.expanded_url|entities\\.url\\.urls\\.expanded_url", names(du), value = T)
  if(length(h) != 0){
    du$url_expanded_url <- combine_list_columns_tweets(h = h, dt = du)
    du$url_expanded_url <- as.character(du$url_expanded_url)
    du <- du[ , !names(du) %in% h]
  }

  ## url_display
  h <- grep("entities\\.url\\.urls\\d+\\.display_url|entities\\.url\\.urls\\.display_url", names(du), value = T)
  if(length(h) != 0){
    du$url_display_url <- combine_list_columns_tweets(h = h, dt = du)
    du$url_display_url <- as.character(du$url_display_url)
    du <- du[ , !names(du) %in% h]
  }

  ## description expanded urls
  h <- grep("entities\\.description\\.urls\\d+\\.expanded_url|entities\\.description\\.urls\\.expanded_url", names(du), value = T)
  if(length(h) != 0){
    du$urls_description_expanded_url <- combine_list_columns_tweets(h = h, dt = du)
    du$urls_description_expanded_url <- as.character(du$urls_description_expanded_url)
    du <- du[ , !names(du) %in% h]
  }

  ## description display urls
  h <- grep("entities\\.description\\.urls\\d+\\.display_url|entities\\.description\\.urls\\.display_url", names(du), value = T)
  if(length(h) != 0){
    du$urls_description_display_url <- combine_list_columns_tweets(h = h, dt = du)
    du$urls_description_display_url <- as.character(du$urls_description_display_url)
    du <- du[ , !names(du) %in% h]
  }

  h <- grep("entities\\.", names(du), value = T)
  if(length(h) != 0){
    du <- du[ , !names(du) %in% h]
  }

  ## get columns names right:
  names(du)[names(du)=="username"] <- "screen_name"
  names(du)[names(du)=="name"] <- "name"
  names(du)[names(du)=="id"] <- "user_id"
  names(du)[names(du)=="created_at"] <- "account_created_at"
  colnames_new <- gsub("public_metrics.","",names(du))
  colnames(du) <- colnames_new

  # ----- split user of time-line and users which are retweeted and quoted -----#
  ## Select Data form Users tweets are quoted and re-tweeted
  dqr <- du[du$user_id != dt$user_id[1], ]
  ## Select Data form User of Time-line only
  du <- du[du$user_id == dt$user_id[1], ]
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
    colnames_pre <- names(dm)
    colnames_pre <- paste0("media_",colnames_pre,"_i")
    colnames(dm) <- colnames_pre
    names(dm)[names(dm) == 'media_media_key_i'] <- 'media_media_key'

    dm <- dm[!duplicated(dm[ , c("media_media_key")]),]

    h <- grep("attachments.media_keys_", names(dt), value = T)
    g <- grep("\\_i", names(dm), value = T)
    f <- gsub("\\_i", "", g)
    for(i in 1:length(h)){
      dt <- data.table::merge.data.table(dt,
                             dm,
                             by.x = h[i],
                             by.y = "media_media_key",
                             all.x = TRUE)
      for(j in 1:length(g)){
        if(i == 1){
          epr_s <- paste0("dt$",f[j]," <- as.character(dt$",g[j],")")
          eval(parse(text=epr_s))
          epr_s <- paste0("dt$",g[j]," <- NULL")
          eval(parse(text=epr_s))
        } else {
          epr_s <- paste0("dt$",f[j]," <- as.character(ifelse(is.na(dt$",g[j],") == T, dt$",f[j],",  paste0(dt$",f[j],",',',dt$",g[j],")))")
          eval(parse(text=epr_s))
          epr_s <- paste0("dt$",g[j]," <- NULL")
          eval(parse(text=epr_s))
        }
      }

    }
    dt <- as.data.frame(dt)
    dt <- dt[ , !names(dt) %in% c(h,"attachments.media_keys")]
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
    colnames_pre <- names(dp)
    colnames_pre <- paste0("places_",colnames_pre,"_i")
    colnames(dp) <- colnames_pre
    names(dp)[names(dp) == 'places_id_i'] <- 'places_id'

    h <- grep("geo.place_id_", names(dt), value = T)
    g <- grep("\\_i", names(dp), value = T)
    f <- gsub("\\_i", "", g)
    for(i in 1:length(h)){
      dt <- data.table::merge.data.table(dt,
                             dp,
                             by.x = h[i],
                             by.y = "places_id",
                             all.x = TRUE)
      for(j in 1:length(g)){
        if(i == 1){
          epr_s <- paste0("dt$",f[j]," <- dt$",g[j])
          eval(parse(text=epr_s))
          epr_s <- paste0("dt$",g[j]," <- NULL")
          eval(parse(text=epr_s))
        } else {
          epr_s <- paste0("dt$",f[j]," <- ifelse(is.na(dt$",g[j],") == T, dt$",f[j],",  paste0(dt$",f[j],",',',dt$",g[j],"))")
          eval(parse(text=epr_s))
          epr_s <- paste0("dt$",g[j]," <- NULL")
          eval(parse(text=epr_s))
        }
      }

    }
    dt <- as.data.frame(dt)
    dt <- dt[ , !names(dt) %in% c(h,"geo.place_id")]
  }
  ##################################################################
  # ----- quoted and retweeted tweets data ----- #
  ##################################################################
  tweet_list <- includes_list$tweets
  for(i in 1:length(tweet_list)){
    tweet_list[[i]]$attachments$media_keys <- gsub("\\,\\s","\\,", toString(na.omit(tweet_list[[i]]$attachments$media_keys)))
  }
  drrqt <- lapply(tweet_list,flattenlist)
  drrqt <- purrr::map(drrqt, as.data.table)
  drrqt <- data.table::rbindlist(drrqt, fill = TRUE)
  drrqt <- as.data.frame(drrqt)

  drrqt <- tweets_transformer(drrqt)


  names(drrqt)[names(drrqt)=="id"] <- "status_id"
  names(drrqt)[names(drrqt)=="author_id"] <- "user_id"
  colnames_new <- gsub("public_metrics.","",names(drrqt))
  colnames(drrqt) <- colnames_new

  # ---- add quotes fields to dt ---- #
  h <- unlist(unique(dt$referenced_tweets_type))
  h <- grep("quoted", h, value = T)
  d_tmp_1 <- dt[dt$referenced_tweets_type %in% h , ]
  if(nrow(d_tmp_1) == 0){
    # No Quotes
  } else {
    helper <- purrr::map(strsplit(unlist(d_tmp_1$referenced_tweets_type), split = ","), as.data.table)
    helper <- data.table::rbindlist(helper, fill = TRUE)
    selector <- grepl("quoted", helper$V1)
    helper <- purrr::map(strsplit(unlist(d_tmp_1$referenced_tweets_id), split = ","), as.data.table)
    helper <- data.table::rbindlist(helper, fill = TRUE)
    helper$selector <- selector
    helper <- helper[helper$selector == TRUE]
    d_tmp_q <- drrqt[drrqt$status_id %in% helper$V1, ]
    d_tmp_q <- d_tmp_q[!duplicated(d_tmp_q$status_id), ]
    #d_tmp_q <- d_tmp_q[(is.na(d_tmp_q$referenced_tweets_id) == T || d_tmp_q$referenced_tweets_id == "quoted"), ]

    h <- c("in_reply_to_user_id", "reply_setings", "attachments.media_keys",
           "attachments.poll_ids", "urls_images1_width", "urls_images1_height",
           "urls_images2_width", "urls_images2_height", "referenced_tweets_type",
           "referenced_tweets_id", "urls_images2_url", "urls_images1_url")
    d_tmp_q <- d_tmp_q[ , !names(d_tmp_q) %in% h]
    colnames_new <- paste0("quoted_",names(d_tmp_q))
    colnames(d_tmp_q) <- colnames_new

    d_tmp_1$quoted_status_id <- helper$V1
    d_tmp_q <- merge(d_tmp_1,d_tmp_q, by = "quoted_status_id")

    # ---- add quotes user fields to dt ---- #
    dq <- dqr[dqr$user_id %in% unique(d_tmp_q$quoted_user_id), ]
    colnames_new <- paste0("quoted_",names(dq))
    colnames(dq) <- colnames_new
  }


  # ---- add retweets fields to dt ---- #
  h <- unique(dt$referenced_tweets_type)
  h <- grep("retweeted", h, value = T)
  d_tmp_2 <- dt[dt$referenced_tweets_type %in% h , ]
  if(nrow(d_tmp_2) == 0){
    # No Retweets
  } else {
    helper <- purrr::map(strsplit(unlist(d_tmp_2$referenced_tweets_type), split = ","), as.data.table)
    helper <- data.table::rbindlist(helper, fill = TRUE)
    selector <- grepl("retweeted", helper$V1)
    helper <- purrr::map(strsplit(unlist(d_tmp_2$referenced_tweets_id), split = ","), as.data.table)
    helper <- data.table::rbindlist(helper, fill = TRUE)
    helper$selector <- selector
    helper <- helper[helper$selector == TRUE]
    d_tmp_r <- drrqt[drrqt$status_id %in% helper$V1, ]
    d_tmp_r <- d_tmp_r[!duplicated(d_tmp_r$status_id), ]

    h <- c("in_reply_to_user_id", "reply_setings", "attachments.media_keys",
           "attachments.poll_ids", "urls_images1_width", "urls_images1_height",
           "urls_images2_width", "urls_images2_height", "referenced_tweets_type",
           "referenced_tweets_id", "urls_images2_url", "urls_images1_url")
    d_tmp_r <- d_tmp_r[ , !names(d_tmp_r) %in% h]
    colnames_new <- paste0("retweet_",names(d_tmp_r))
    colnames(d_tmp_r) <- colnames_new

    d_tmp_2$retweet_status_id <- helper$V1
    d_tmp_r <- merge(d_tmp_2,d_tmp_r, by = "retweet_status_id")

    # ---- add retweets user fields to dt ---- #
    dr <- dqr[dqr$user_id %in% unique(d_tmp_r$retweet_user_id), ]
    colnames_new <- paste0("retweet_",names(dr))
    colnames(dr) <- colnames_new
  }


  # ---- add normal tweets and retweets and quotes together ---- #
  #h <- c(unique(d_tmp_r$status_id),unique(d_tmp_q$status_id))
  #d_tmp <- dt[!dt$status_id %in% h , ]
  eq_cols <- colnames(dt)
  if(nrow(d_tmp_2) == 0){
    # No Re-Tweets to add
  } else {
    dt <- merge(dt, d_tmp_r, by = eq_cols, all.x = TRUE, all.y = TRUE)
    dt <- merge(dt, dr, by = "retweet_user_id", all.x = TRUE, all.y = TRUE)
    rm(d_tmp_r,d_tmp_2,dr)
  }

  if(nrow(d_tmp_1)==0){
    # No Quoted Tweets to add
  } else {
    dt <- merge(dt, d_tmp_q, by = eq_cols, all.x = TRUE, all.y = TRUE)
    dt <- merge(dt, dq, by = "quoted_user_id", all.x = TRUE, all.y = TRUE)
    rm(d_tmp_q,d_tmp_1,dq)
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
    dt <- dt[ , !names(dt) %in% h]
  }

  h <- unique(dt$referenced_tweets_type)

  dt$is_retweet <- ifelse(grepl("retweeted", dt$referenced_tweets_type) == T, 1, 0 )
  dt$is_quote <- ifelse(grepl("quoted", dt$referenced_tweets_type) == T, 1, 0 )

  dt$referenced_tweets_id <- NULL
  dt$referenced_tweets_type <- NULL

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
