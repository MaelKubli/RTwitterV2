# ' @title: RTweetV2 Function transforming the tweets

#' This function is a sub function transforming many list type columns in the twitter data to a simple column with string of comma delimited entries
#' @param dt data table or data frame

#' @return a data frame or data table
#' @export
#'
#' @examples
#' \dontrun{
#' }

# transform_tweets_data
tweets_transformer <- function(dt){

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

  # Remove cols we have no use for
  h <- grep("\\.start$|\\.end$", names(dt), value = T)
  dt <- dt[ , !names(dt) %in% h]

  # domain id
  h <- grep("context_annotations\\.domain\\.id|context_annotations\\d+\\.domain\\.id", names(dt), value = T)
  if(length(h) != 0){
    dt$domain_id <- combine_list_columns_tweets(h=h,dt=dt)
    dt$domain_id <- as.character(dt$domain_id)
    dt <- dt[ , !names(dt) %in% h]
  }

  # domain name
  h <- grep("context_annotations\\.domain\\.name|context_annotations\\d+\\.domain\\.name", names(dt), value = T)
  if(length(h) != 0){
    dt$domain_name <- combine_list_columns_tweets(h=h,dt=dt)
    dt$domain_name <- as.character(dt$domain_name)
    dt <- dt[ , !names(dt) %in% h]
  }

  # domain description
  h <- grep("context_annotations\\.domain\\.description|context_annotations\\d+\\.domain\\.description", names(dt), value = T)
  if(length(h) != 0){
    dt$domain_description <- combine_list_columns_tweets(h=h,dt=dt)
    dt$domain_description <- as.character(dt$domain_description)
    dt <- dt[ , !names(dt) %in% h]
  }

  # entity id
  h <- grep("context_annotations\\.entity\\.id|context_annotations\\d+\\.entity\\.id", names(dt), value = T)
  if(length(h) != 0){
    dt$entity_id <- combine_list_columns_tweets(h=h,dt=dt)
    dt$entity_id <- as.character(dt$entity_id)
    dt <- dt[ , !names(dt) %in% h]
  }

  # entity name
  h <- grep("context_annotations\\.entity\\.name|context_annotations\\d+\\.entity\\.name", names(dt), value = T)
  if(length(h) != 0){
    dt$entity_name <- combine_list_columns_tweets(h=h,dt=dt)
    dt$entity_name <- as.character(dt$entity_name)
    dt <- dt[ , !names(dt) %in% h]
  }

  # entity description
  h <- grep("context_annotations\\.entity\\.description|context_annotations\\d+\\.entity\\.description", names(dt), value = T)
  if(length(h) != 0){
    dt$entity_description <- combine_list_columns_tweets(h=h,dt=dt)
    dt$entity_description <- as.character(dt$entity_description)
    dt <- dt[ , !names(dt) %in% h]
  }

  # mentions_screen_name
  h <- grep("entities\\.mentions\\d+\\.username|entities\\.mentions\\.username", names(dt), value = T)
  if(length(h) != 0){
    dt$mentions_screen_name <- combine_list_columns_tweets(h=h,dt=dt)
    dt$mentions_screen_name <- as.character(dt$mentions_screen_name)
    dt <- dt[ , !names(dt) %in% h]
  }

  #entities probability
  h <- grep("entities\\.annotations\\.probability|entities\\.annotations\\d+\\.probability", names(dt), value = T)
  if(length(h) != 0){
    dt$entities_probability <- combine_list_columns_tweets(h=h,dt=dt)
    dt$entities_probability <- as.character(dt$entities_probability)
    dt <- dt[ , !names(dt) %in% h]
  }

  #entities type
  h <- grep("entities\\.annotations\\.type|entities\\.annotations\\d+\\.type", names(dt), value = T)
  if(length(h) != 0){
    dt$entities_type <- combine_list_columns_tweets(h=h,dt=dt)
    dt$entities_type <- as.character(dt$entities_type)
    dt <- dt[ , !names(dt) %in% h]
  }

  #entities normalized text
  h <- grep("entities\\.annotations\\.normalized_text|entities\\.annotations\\d+\\.normalized_text", names(dt), value = T)
  if(length(h) != 0){
    dt$entities_normalizes_text <- combine_list_columns_tweets(h=h,dt=dt)
    dt$entities_normalizes_text <- as.character(dt$entities_normalizes_text)
    dt <- dt[ , !names(dt) %in% h]
  }

  # entities urls url
  h <- grep("entities\\.urls\\.url|entities\\.urls\\d+\\.url", names(dt), value = T)
  if(length(h) != 0){
    dt$urls_url <- combine_list_columns_tweets(h=h,dt=dt)
    dt$urls_url <- as.character(dt$urls_url)
    dt <- dt[ , !names(dt) %in% h]
  }

  # entities urls expanded url
  h <- grep("entities\\.urls\\.expanded_url|entities\\.urls\\d+\\.expanded_url", names(dt), value = T)
  if(length(h) != 0){
    dt$urls_expanded_url <- combine_list_columns_tweets(h=h,dt=dt)
    dt$urls_expanded_url <- as.character(dt$urls_expanded_url)
    dt <- dt[ , !names(dt) %in% h]
  }

  # entities urls title
  h <- grep("entities\\.urls\\.title|entities\\.urls\\d+\\.title", names(dt), value = T)
  if(length(h) != 0){
    dt$urls_title <- combine_list_columns_tweets(h=h,dt=dt)
    dt$urls_title <- as.character(dt$urls_title)
    dt <- dt[ , !names(dt) %in% h]
  }

  # entities urls description
  h <- grep("entities\\.urls\\.description|entities\\.urls\\d+\\.description", names(dt), value = T)
  if(length(h) != 0){
    dt$urls_description <- combine_list_columns_tweets(h=h,dt=dt)
    dt$urls_description <- as.character(dt$urls_description)
    dt <- dt[ , !names(dt) %in% h]
  }

  # entities unwound url
  h <- grep("entities\\.urls\\.unwound_url|entities\\.urls\\d+\\.unwound_url", names(dt), value = T)
  if(length(h) != 0){
    dt$urls_unwound_url <- combine_list_columns_tweets(h=h,dt=dt)
    dt$urls_unwound_url <- as.character(dt$urls_unwound_url)
    dt <- dt[ , !names(dt) %in% h]
  }

  # entities urls images1 url
  h <- grep("entities\\.urls\\.images1\\.url|entities\\.urls\\d+\\.images1\\.url", names(dt), value = T)
  if(length(h) != 0){
    dt$urls_images1_url <- combine_list_columns_tweets(h=h,dt=dt)
    dt$urls_images1_url <- as.character(dt$urls_images1_url)
    dt <- dt[ , !names(dt) %in% h]
  }

  # entities urls images1 url
  h <- grep("entities\\.urls\\.images2\\.url|entities\\.urls\\d+\\.images2\\.url", names(dt), value = T)
  if(length(h) != 0){
    dt$urls_images2_url <- combine_list_columns_tweets(h=h,dt=dt)
    dt$urls_images2_url <- as.character(dt$urls_images2_url)
    dt <- dt[ , !names(dt) %in% h]
  }

  # entities urls image1 width
  h <- grep("entities\\.urls\\.images1\\.width|entities\\.urls\\d+\\.images1\\.width", names(dt), value = T)
  if(length(h) != 0){
    dt$urls_images1_width <- combine_list_columns_tweets(h=h,dt=dt)
    dt$urls_images1_width <- as.character(dt$urls_images1_width)
    dt <- dt[ , !names(dt) %in% h]
  }

  # entities urls image1 height
  h <- grep("entities\\.urls\\d+\\.images1\\.height|entities\\.urls\\.images1\\.height", names(dt), value = T)
  if(length(h) != 0){
    dt$urls_images1_height <- combine_list_columns_tweets(h=h,dt=dt)
    dt$urls_images1_height <- as.character(dt$urls_images1_height)
    dt <- dt[ , !names(dt) %in% h]
  }

  # entities urls image2 width
  h <- grep("entities\\.urls\\.images2\\.width|entities\\.urls\\d+\\.images2\\.width", names(dt), value = T)
  if(length(h) != 0){
    dt$urls_images2_width <- combine_list_columns_tweets(h=h,dt=dt)
    dt$urls_images2_width <- as.character(dt$urls_images2_width)
    dt <- dt[ , !names(dt) %in% h]
  }

  # entities urls image2 heigth
  h <- grep("entities\\.urls\\d+\\.images2\\.height|entities\\.urls\\.images2\\.height", names(dt), value = T)
  if(length(h) != 0){
    dt$urls_images2_height <- combine_list_columns_tweets(h=h,dt=dt)
    dt$urls_images2_height <- as.character(dt$urls_images2_height)
    dt <- dt[ , !names(dt) %in% h]
  }

  # entities urls status
  h <- grep("entities\\.urls\\d+\\.status|entities\\.urls\\.status", names(dt), value = T)
  if(length(h) != 0){
    dt <- dt[ , !names(dt) %in% h]
  }

  # entities hashtags
  h <- grep("entities\\.hashtags\\.tag|entities\\.hashtags\\d+\\.tag", names(dt), value = T)
  if(length(h) != 0){
    dt$hashtags <- combine_list_columns_tweets(h=h,dt=dt)
    dt$hashtags <- as.character(dt$hashtags)
    dt <- dt[ , !names(dt) %in% h]
  }

  #display_url
  h <- grep("entities\\.urls\\.display_url|entities\\.urls\\d+\\.display_url", names(dt), value = T)
  if(length(h) != 0){
    dt$display_url <- combine_list_columns_tweets(h=h,dt=dt)
    dt$display_url <- as.character(dt$display_url)
    dt <- dt[ , !names(dt) %in% h]
  }

  #referenced tweets type
  h <- grep("referenced_tweets\\.type|referenced_tweets\\d+\\.type", names(dt), value = T)
  if(length(h) != 0){
    dt$referenced_tweets_type <- combine_list_columns_tweets(h=h,dt=dt)
    dt <- dt[ , !names(dt) %in% h]
  }

  #referenced tweets id
  h <- grep("referenced_tweets\\.id|referenced_tweets\\d+\\.id", names(dt), value = T)
  if(length(h) != 0){
    dt$referenced_tweets_id <- combine_list_columns_tweets(h=h,dt=dt)
    dt <- dt[ , !names(dt) %in% h]
  }

  return(dt)
}
