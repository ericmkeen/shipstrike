#' Gather and concatentate outcome `RData` files
#'
#' This is not typically called by the user and currently deprecated.
#'
#' @param outcome_dir Path (relative or absolute) to directory containing outcome files.
#'
#' @param verbose Boolean; print updates to Console?
#'
#' @return A `data.frame`
#' @export
#'
gather_outcomes <- function(outcome_dir, verbose=TRUE){
  #outcome_dir <- 'tests/fw/impacts_2030/outcomes/'

  mr <- data.frame()
  for(j in 1:length(outcome_dir)){
    outcome_dirj <- outcome_dir[j]
    message('--- ',outcome_dirj)
    lf <- list.files(outcome_dirj)
    (lf <- paste0(outcome_dirj,lf))
    for(i in 1:length(lf)){
      (lfi <- lf[i])
      if(verbose){message('--- --- ',lfi)}
      mri <- readRDS(lfi)
      mri$folder <- outcome_dirj
      mr <- rbind(mr, mri)
    }
  }

  mr$channel[mr$channel == 'CAA'] <- 'Caamano'
  mr$channel[mr$channel == 'EST'] <- 'Estevan'
  mr$channel[mr$channel == 'CMP'] <- 'Campania'
  mr$channel[mr$channel == 'SQU'] <- 'Squally'
  mr$channel[mr$channel == 'WHA'] <- 'Whale'
  mr$channel[mr$channel == 'WRI'] <- 'Wright'
  mr$channel[mr$channel == 'MCK'] <- 'McKay'
  mr$channel[mr$channel == 'VER'] <- 'Verney'
  mr$channel <- factor(mr$channel, levels=c('Caamano','Estevan','Campania','Squally','Whale','Wright','McKay','Verney'))

  head(mr)
  nrow(mr)
  return(mr)
}
