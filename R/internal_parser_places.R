#' Parser Helper Places List

#' This function reads lines from raw json files saved from the streaming endpoints
#' @param dp data frame containing places objects returned by API
#' @param dt data frame with current data frame to return

#' @return data.frame

#' @import httr httpuv RCurl ROAuth data.table readr
#' @importFrom purrr map

#' @noRd
##################################################################################################
# Transforms list object with tweets data into a data frame
##################################################################################################
internal_parser_places <- function(dp, dt){

  colnames_pre <- names(dp)
  colnames_pre <- paste0("places_",colnames_pre,"_i")
  colnames(dp) <- colnames_pre
  names(dp)[names(dp) == 'places_id_i'] <- 'places_id'

  h <- grep("geo.place_id_", names(dt), value = T)
  g <- grep("\\_i", names(dp), value = T)
  f <- gsub("\\_i", "", g)

  # Distinct Place ID's
  dp <- dp[!duplicated(dp$places_id), ]

  if(length(h) == 0) {
    dt <- as.data.frame(dt)
    dt <- dt[ , !names(dt) %in% c("geo.place_id")]
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
    dt <- as.data.frame(dt)
    dt <- dt[ , !names(dt) %in% c(h,"geo.place_id")]
  }

  return(dt)
}
