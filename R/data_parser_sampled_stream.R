# ' @title: RTweetV2 Function data parser sampled stream

#' This function is a sub function parsing the data returned from the API
#' @param results_data list with results from the API call

#' @return data frame
#' @export
#'

#' @import httr httpuv RCurl ROAuth data.table readr
#' @importFrom purrr map
#'
##################################################################################################
# Parse full_archive_search
##################################################################################################
data_parser_sampled_stream <- function(results_data){


  for(i in 1:length(results_data)){
    results_data[[i]]$data$attachments$media_keys <- gsub("\\,\\s","\\,", toString(na.omit(results_data[[i]]$data$attachments$media_keys)))
  }

  dat <- lapply(results_data,flattenlist)
  dat <- purrr::map(dat, as.data.table)
  dat <- data.table::rbindlist(dat, fill = TRUE)
  dat <- as.data.frame(dat)

  checker <- grep("withheld", colnames(dat))
  if(length(checker) != 0){
    dat <- dat[, -grep("withheld", colnames(dat))]
  }

  dt <- dat

  nms <- names(dt)
  dt <- dt[ grepl("data.", nms)]
  colnames(dt) <- gsub("data.", "",names(dt))

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
  # ----- users data ----- #
  ##################################################################
  ## users fields
  du <- dat

  nms <- names(du)
  du <- du[ grepl("includes.users", nms)]
  colnames(du) <- gsub("includes.users", "",names(du))

  ## Remove cols we have no use for
  h <- grep("\\.start$|\\.end$", names(du), value = T)
  du <- du[ , !names(du) %in% h]

  # Transform data   starting with .xxx and 1.xxx etc. as they are complementary to each other...
  max_num <- unique(unlist(regmatches(names(du), gregexpr("^\\d{1,2}.|\\.",  names(du)))))
  max_num <- gsub("\\.", "\\\\\\.", max_num)
  for(n in 1:length(max_num)){
    h <- grep(pattern = paste0("^", max_num[n]), names(du), value = T)
    tmp <- du[ , names(du) %in% h]

    tmp <- tmp[rowSums(is.na(tmp)) != ncol(tmp), ]
    colnames(tmp) <- gsub(paste0("^",max_num[n]), "", names(tmp))
    tmp <- as.data.table(tmp)

    if(n == 1){
      du_fin <- tmp
    } else {
      du_fin <- data.table::rbindlist(list(du_fin, tmp), fill = T)
    }
  }

  du <- as.data.frame(du_fin)
  rm(du_fin)


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
  colnames(du) <- gsub("^\\.","",names(du))
  names(du)[names(du)=="username"] <- "screen_name"
  names(du)[names(du)=="name"] <- "name"
  names(du)[names(du)=="id"] <- "user_id"
  names(du)[names(du)=="created_at"] <- "account_created_at"
  colnames_new <- gsub("public_metrics.","",names(du))
  colnames(du) <- colnames_new

  # ----- split user of time-line and users which are retweeted and quoted -----#
  ## Select Data form Users tweets are quoted and re-tweeted
  dqr <- du[!du$user_id %in% unique(dt$user_id), ]
  ## Select Data form User of Time-line only
  du <- du[du$user_id %in% unique(dt$user_id), ]
  ## Add User Fields to Tweets
  dt <- data.table::merge.data.table(dt,du, by = c("user_id"))
  ##################################################################
  # ----- media add-on for dt ----- #
  ##################################################################
  dm <- stream_list_prep_i(dat = dat, string = "includes.media")

  # Transform data   starting with .xxx and 1.xxx etc. as they are complementary to each other...
  max_num <- unique(unlist(regmatches(names(dm), gregexpr("^\\d{1,2}.|\\.",  names(dm)))))
  max_num <- gsub("\\.", "\\\\\\.", max_num)
  for(n in 1:length(max_num)){
    h <- grep(pattern = paste0("^", max_num[n]), names(dm), value = T)
    tmp <- dm[ , names(dm) %in% h]

    tmp <- tmp[rowSums(is.na(tmp)) != ncol(tmp), ]
    colnames(tmp) <- gsub(paste0("^",max_num[n]), "", names(tmp))
    tmp <- as.data.table(tmp)

    if(n == 1){
      dm_fin <- tmp
    } else {
      dm_fin <- data.table::rbindlist(list(dm_fin, tmp), fill = T)
    }
  }

  dm <- as.data.frame(dm_fin)
  rm(dm_fin)

  if(nrow(dm)>=1){
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
  dp <- stream_list_prep_i(dat = dat, string = "includes.places")

  # Transform data   starting with .xxx and 1.xxx etc. as they are complementary to each other...
  max_num <- unique(unlist(regmatches(names(dp), gregexpr("^\\d{1,2}.|\\.",  names(dp)))))
  max_num <- gsub("\\.", "\\\\\\.", max_num)
  for(n in 1:length(max_num)){
    h <- grep(pattern = paste0("^", max_num[n]), names(dp), value = T)
    tmp <- dp[ , names(dp) %in% h]

    tmp <- tmp[rowSums(is.na(tmp)) != ncol(tmp), ]
    colnames(tmp) <- gsub(paste0("^",max_num[n]), "", names(tmp))
    tmp <- as.data.table(tmp)

    if(n == 1){
      dp_fin <- tmp
    } else {
      dp_fin <- data.table::rbindlist(list(dp_fin, tmp), fill = T)
    }
  }

  dp <- as.data.frame(dp_fin)
  rm(dp_fin)

  if(nrow(dp)>=1){
    h <- grep("geo\\.bbox\\d", names(dp), value = T)
    if(length(h) != 0){
      dp$geo_bbox <- combine_list_columns_tweets(h=h,dt=dp)
      dp$geo_bbox <- as.character(dp$geo_bbox)
      dp <- dp[ , !names(dp) %in% h]
    }

    colnames_pre <- names(dp)
    colnames_pre <- paste0("places_",colnames_pre,"_i")
    colnames(dp) <- colnames_pre
    names(dp)[names(dp) == 'places_id_i'] <- 'places_id'

    h <- grep("geo.place_id_", names(dt), value = T)
    g <- grep("\\_i", names(dp), value = T)
    f <- gsub("\\_i", "", g)
    if(length(h) == 0) {

    } else {
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
    }
    dt <- as.data.frame(dt)
    dt <- dt[ , !names(dt) %in% c(h,"geo.place_id")]
  }
  ##################################################################
  # ----- quoted and retweeted tweets data ----- #
  ##################################################################
  drrqt <- stream_list_prep_i(dat = dat, string = "includes.tweets")

  # Transform data   starting with .xxx and 1.xxx etc. as they are complementary to each other...
  max_num <- unique(unlist(regmatches(names(drrqt), gregexpr("^\\d{1,2}.|^\\.",  names(drrqt)))))
  max_num <- gsub("\\.", "\\\\\\.", max_num)
  for(n in 1:length(max_num)){
    h <- grep(pattern = paste0("^", max_num[n]), names(drrqt), value = T)
    tmp <- drrqt[ , names(drrqt) %in% h]

    tmp <- tmp[rowSums(is.na(tmp)) != ncol(tmp), ]
    colnames(tmp) <- gsub(paste0("^",max_num[n]), "", names(tmp))
    tmp <- as.data.table(tmp)

    if(n == 1){
      drrqt_fin <- tmp
    } else {
      drrqt_fin <- data.table::rbindlist(list(drrqt_fin, tmp), fill = T)
    }
  }

  drrqt <- as.data.frame(drrqt_fin)
  rm(drrqt_fin)

  colnames(drrqt) <- gsub("^\\.","",names(drrqt))

  if(nrow(drrqt) < 1) {
    location <- TRUE
  } else {
    location <- FALSE

    h <- grep("attachments\\.media_keys", names(drrqt), value = T)
    h2 <- grep("attachments\\.media_keys\\d", names(drrqt), value = T)
    if(length(h) != 0){
      drrqt$attachments.media_keys <- combine_list_columns_tweets(h=h,dt=drrqt)
      drrqt$attachments.media_keys <- as.character(drrqt$attachments.media_keys)
      drrqt <- drrqt[ , !names(drrqt) %in% h2]
    }

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
      dq <- unique(dq)
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
      dr <- unique(dr)
    }


    # ---- add normal tweets and retweets and quotes together ---- #
    #h <- c(unique(d_tmp_r$status_id),unique(d_tmp_q$status_id))
    #d_tmp <- dt[!dt$status_id %in% h , ]
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

  #dt$referenced_tweets_id <- NULL
  #dt$referenced_tweets_type <- NULL
  dt <- dt[!duplicated(dt[,c("status_id")]),]
  return(dt)
}
