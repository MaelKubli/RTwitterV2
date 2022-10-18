# ' @title: RTweetV2 Function to add a rule to the filtered stream endpoint

#' This function allows you to collect tweets from the 1 \% sampled stream from twitter
#' @param token string of the bearer token from your personal twitter API access
#' @param value string with rule value
#' @param tag string with rule tag
#
#' @return a list
#' @export
#'
#' @examples
#' \dontrun{
#' filtered_stream_add_rule(token = your_token,
#'                          value = "",
#'                          tag = "")
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
filtered_stream_add_rule <- function(token = "",
                                     value = "",
                                     tag = ""){
  # Check if Bearer Token is set:
  if(is.na(token) | nchar(token) < 90){
    stop("Please add the Bearer Token of your projects dashboard!\n")
  }

  #Add token to header
  token_fin <- paste0("Bearer ", token)
  headers = c(
    'Authorization' = token_fin,
    'Content-Type' = 'application/json'
  )

  #Add value and tag to params
  rule <- paste0("{
                    \"add\": [
                      {
                        \"value\": \"",value,"\",
                        \"tag\": \"",tag,"\"
                      }
                    ]
                  }")

  response <- httr::POST(url = "https://api.twitter.com/2/tweets/search/stream/rules",
                         httr::add_headers(.headers=headers),
                         body = rule)

  if(response$status_code != 201){
    return(response$status_code)
    results_list <- jsonlite::fromJSON(httr::content(response, "text"), simplifyDataFrame =  F)
    cat(paste0(results_list$detail,"\n"))
  } else {
    results_list <- jsonlite::fromJSON(httr::content(response, "text"), simplifyDataFrame =  F)
    return(results_list)
  }
}
