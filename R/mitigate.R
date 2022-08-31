#' Apply mitigation filters to a list of outcomes
#'
#' @param outcomes desc
#' @param mitigation_filters desc
#'
#' @return desc
#' @export
#' @import dplyr
#'
mitigate <- function(outcomes, mitigation_filters){
  mr <- outcomes

  if(!is.null(mitigation_filters)){

    bads <- c()
    # Loop through each mitigation filter sublist
    mfi = 1
    for(mfi in 1:length(mitigation_filters)){
      mf <- mitigation_filters[[mfi]] # for convenience
      mf
      mf %>% names

      vessels <- unique(mr$vessel)
      if('vessel' %in% names(mf)){vessels <- mf$vessel}
      vessels

      months <- unique(mr$month)
      if('month' %in% names(mf)){months <- mf$month}
      months

      channels <- unique(mr$channel)
      if('channel' %in% names(mf)){channels <- mf$channel}
      channels

      diels <- unique(mr$diel)
      if('diel' %in% names(mf)){diels <- mf$diel}
      diels

      (vi <- which(mr$vessel %in% mf$vessel)) %>% length
      (mi <- which(mr$month %in% mf$month)) %>% length
      (ci <- which (mr$channel %in% channels)) %>% length
      (di <- which(mr$diel %in% diels)) %>% length

      (badi <- Reduce(intersect, list(vi, mi, ci, di))) %>% length
      bads <- c(bads, badi)
    } # end of mitigation_filters loop

    bads %>% length
    (bads <- unique(bads)) %>% length
    bads

    # Get columns that represent actual predictions
    cols <- 8:ncol(mr)
    names(mr[cols])

    if(!'displace' %in% names(mf)){ mf$displace <- FALSE }
    mf$displace

    if(mf$displace){
      # Randomly select good rows that will be used to replace the bad ones
      goods <- 1:nrow(mr)
      goods <- goods[-bads]
      length(goods)
      # now randomly select some of these 'goods' -- the same number of bads
      touse <- sample(goods, size=length(bads), replace = TRUE)
      length(touse)

      # Replace bads with these typical outomes from other months
      mr[bads, cols] <-  mr[touse, cols]

    }else{
      # Instead of displacing,
      # just coerce values to zero
      names(mr)
      mr[bads, cols] <- 0
    }

  } # end of if mitigation_filters isn't null

  # Number of rows should be unchanged
  nrow(mr)

  return(mr)
}
