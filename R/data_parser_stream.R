# ' @title: RTweetV2 Function data parser sampled stream

#' This function is a sub function parsing the data returned from the API
#' @param results_data list with results from the API call
#' @param filtered logical, indicating which stream endpoint was used in the data (filtered or sampled)

#' @return data frame
#' @export
#'

#' @import httr httpuv RCurl ROAuth data.table readr
#' @importFrom purrr map
#'
##################################################################################################
# Parse full_archive_search
##################################################################################################
data_parser_stream <- function(results_data, filtered){

  for(i in 1:length(results_data)){
    results_data[[i]]$data$attachments$media_keys <- gsub("\\,\\s","\\,", toString(na.omit(results_data[[i]]$data$attachments$media_keys)))
  }

  dat <- lapply(results_data,flattenlist)         #Slow (make faster)
  dat <- purrr::map(dat, as.data.table)           #Slow (make faster)
  dat <- data.table::rbindlist(dat, fill = TRUE)  #Slow (make faster)
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
    dt <- internal_parser_media(dm = dm, dt = dt)
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

    dt <- internal_parser_places(dp,dt)
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

    dt <- internal_parser_rt_qt(drrqt = drrqt, dqr = dqr, dt = dt)
  }

  ##################################################################
  # Filtered Stream
  ##################################################################
  if(filtered == T){
    filtered_dat <- stream_list_prep_i(dat = dat, string = "matching_|data.id")
    filtered_dat <- setnames(filtered_dat, c("","rules.id", "rules.tag"), c("status_id", "filter_rules_id", "filter_rules_tag"))

    dt <- merge(dt, filtered_dat, by = "status_id", all.x = TRUE, all.y = FALSE)
    rm(filtered_dat)
    dt <- as.data.frame(dt)
  } else {
    # Nothing to do since it is a sampled stream result
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

  dt <- dt[!duplicated(dt[,c("status_id")]),]
  return(dt)
}
