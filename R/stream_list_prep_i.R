# ' @title: RTweetV2 Function to subset list of streamed tweets for information needed in parse function

#' This function is a sub function combining listed columns into a string for simpler handling in a data frame
#' @param dat data frame of tweets from stream
#' @param string character string with pattern of columns to deal with

#' @return a data frame
#' @export
#'
#'
#' @importFrom purrr map
#' @importFrom data.table rbindlist

# combine listed columns to one column
stream_list_prep_i <- function(dat = dat, string = string){
  #dat <- lapply(results_data,flattenlist)
  #dat <- purrr::map(dat, as.data.table)
  #dat <- data.table::rbindlist(dat, fill = TRUE)
  #dat <- as.data.frame(dat)
  nms <- names(dat)
  dat <- dat[ grepl(string, nms)]
  checker <- grep("withheld", colnames(dat))
  if(length(checker) != 0){
    dat <- dat[, -grep("withheld", colnames(dat))]
  }
  colnames(dat) <- gsub(pattern = string, "",names(dat))
  dat <- dat[rowSums(is.na(dat)) != ncol(dat), ]
  return(dat)
}
