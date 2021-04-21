# ' @title: RTweetV2 Function flattenlist

#' This function is a sub function helping make a list flat.
#' @param x list with elements from the twitter API

#' @return a list
#' @export
#'
#' @examples
#' \dontrun{
#' }


# flatten list of lists where lists are nested within and of unequal length!
flattenlist <- function(x){
  morelists <- sapply(x, function(xprime) class(xprime)[1]=="list")
  out <- c(x[!morelists], unlist(x[morelists], recursive=FALSE))
  if(sum(morelists)){
    Recall(out)
  }else{
    return(out)
  }
}
