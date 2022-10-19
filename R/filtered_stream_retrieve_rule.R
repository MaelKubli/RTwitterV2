# ' @title: RTweetV2 Function to get all rules stored on your filtered stream endpoint

#' This function allows you to collect tweets from the 1 \% sampled stream from twitter
#' @param token string of the bearer token from your personal twitter API access
#'
#
#' @return a list
#' @export
#'
#' @examples
#' \dontrun{
#' filtered_stream_get_rules(token = your_token,)
#'
#' }
#'
#' @import httr httpuv RCurl ROAuth data.table readr
#' @importFrom stats na.omit
#' @importFrom jsonlite fromJSON toJSON
#' @importFrom progress progress_bar
#' @importFrom curl curl
#' @importFrom rlang has_name
##################################################################################################
# Add a new rule to the filtered stream endpoint
##################################################################################################
filtered_stream_get_rules <- function(token = ""){
  # Check if Bearer Token is set:
  if(is.na(token) | nchar(token) < 90){
    stop("Please add the Bearer Token of your projects dashboard!\n")
  }

  #Add token to header
  token_fin <- paste0("Bearer ", token)
  headers = c(
    'Authorization' = token_fin
  )


  response <- httr::GET(url = "https://api.twitter.com/2/tweets/search/stream/rules",
                         httr::add_headers(.headers=headers))

  if(response$status_code != 200){
    return(response$status_code)
  } else {
    results_list <- jsonlite::fromJSON(httr::content(response, "text"), simplifyDataFrame =  F)
    return(results_list)
  }
}
