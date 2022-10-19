#' Parser Helper Retweet / Quote Data

#' This function reads lines from raw json files saved from the streaming endpoints
#' @param drrqt data frame retweet and quote objects
#' @param dqr data frame retweet and quote objects
#' @param dt data frame which is constructed

#' @return data.frame

#' @import httr httpuv RCurl ROAuth data.table readr
#' @importFrom purrr map

#' @noRd
##################################################################################################
# Transforms data frame into nice data frame ...
##################################################################################################
internal_parser_rt_qt <- function(drrqt, dqr, dt){

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
    #dq <- unique(dq)
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
    #dr <- unique(dr)
  }


  # ---- add normal tweets and retweets and quotes together ---- #
  eq_cols <- colnames(dt)
  eq_cols <- eq_cols[! eq_cols %in% c("referenced_tweets_id", "referenced_tweets_type")]
  if(nrow(d_tmp_2) == 0){
    # No Re-Tweets to add
  } else {
    d_tmp_r$referenced_tweets_id <- NULL
    d_tmp_r$referenced_tweets_type <- NULL
    dt <- merge(dt, d_tmp_r, by = eq_cols, all.x = TRUE, all.y = TRUE)
    dt <- merge(dt, dr, by = "retweet_user_id", all.x = TRUE, all.y = TRUE)
    rm(d_tmp_r,d_tmp_2,dr)
  }

  if(nrow(d_tmp_1)==0){
    # No Quoted Tweets to add
  } else {
    d_tmp_q$referenced_tweets_id <- NULL
    d_tmp_q$referenced_tweets_type <- NULL
    dt <- merge(dt, d_tmp_q, by = eq_cols, all.x = TRUE, all.y = TRUE)
    dt <- merge(dt, dq, by = "quoted_user_id", all.x = TRUE, all.y = TRUE)
    rm(d_tmp_q,d_tmp_1,dq)
  }
  return(dt)
}
