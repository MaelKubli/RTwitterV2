# ' @title: RTweetV2 Function handling data as a data frame in R

#' This function is a sub function parsing the data returned from the API
#' @param results_data list with results from the API call

#' @return data frame
#' @export
#'

#' @import httr httpuv RCurl ROAuth data.table readr
#' @importFrom purrr map
##################################################################################################
# Parse get_users_v2 Data
##################################################################################################
data_parser_users <- function(results_data){

  if(is.null(results_data$name)==T){
    results_data <- results_data
  } else {
    results_data <- list(results_data)
  }

  du <- lapply(results_data,flattenlist)
  du <- purrr::map(du, as.data.table)
  du <- data.table::rbindlist(du, fill = TRUE)
  du <- as.data.frame(du)
  ## Remove cols we have no use for
  h <- grep("\\.start$|\\.end$", names(du), value = T)
  du <- du[ , !names(du) %in% h]

  du <- internal_parser_user(du)
  du <- du[ , order(names(du))]

  return(du)
}
