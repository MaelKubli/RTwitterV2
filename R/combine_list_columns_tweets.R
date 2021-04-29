# ' @title: RTweetV2 Function combine lists of columns of tweets

#' This function is a sub function combining listed columns into a string for simpler handling in a data frame
#' @param h string containing the column name
#' @param dt data table or data frame

#' @return a vector
#' @export
#'
#'
#' @importFrom stats na.omit

# combine listed columns to one column
combine_list_columns_tweets <- function(h, dt){
  if(length(h) == 1){
    h_ret <- lapply(dt[, h], function(x){y <- gsub("\\,\\s","\\,", toString(na.omit(x)))
    y <- ifelse(y == "", NA, y)
    return(y)})
  } else {
    h_ret <- apply(dt[, h], 1, function(x){y <- gsub("\\,\\s","\\,", toString(na.omit(x)))
    y <- ifelse(y == "", NA, y)
    return(y)})
  }
}
