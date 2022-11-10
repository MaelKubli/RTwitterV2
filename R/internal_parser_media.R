#' Parser Helper Media List

#' This function reads lines from raw json files saved from the streaming endpoints
#' @param dm data frame containing media objects returned by API
#' @param dt data frame with current data frame to return

#' @return data.frame

#' @import httr httpuv RCurl ROAuth data.table readr
#' @importFrom purrr map

#' @noRd
##################################################################################################
# Transforms list object with tweets data into a data frame
##################################################################################################

internal_parser_media <- function(dm, dt){
  colnames_pre <- names(dm)
  colnames_pre <- paste0("media_",colnames_pre,"_i")
  colnames(dm) <- colnames_pre
  names(dm)[names(dm) == 'media_media_key_i'] <- 'media_media_key'

  #dm <- dm[!duplicated(dm[ , c("media_media_key")]),]
  dm <- dm[!duplicated(dm$media_media_key), ]

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

  return(dt)
}
