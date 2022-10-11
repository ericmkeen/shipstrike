#' Melt the result of `predict_outcomes()` to use with `tidyverse` verbs.
#'
#' This function may be handy if you want to conduct your own tailored analyses.
#' It melts the results data into a tidyverse-friendly format in which every row
#' is a single prediction. Every function above calls this function internally.
#'
#' @param mr A `data.frame` of results from the `shipstrike::gather_outcomes()` function.
#' @param events A character vector of outcome types to retain in the melted dataset.
#'
#' @return A melted `data.frame`
#' @export
#' @import dplyr
#'
outcome_melt <- function(mr,
                         events = c('cooccurrence',
                                    'encounter',
                                    'surface', 'surface2',
                                    'collision1.1','collision1.2','collision1.3','collision1.4',
                                    'collision2.1','collision2.2','collision2.3','collision2.4',
                                    'mortality1.1','mortality1.2','mortality1.3','mortality1.4',
                                    'mortality2.1','mortality2.2','mortality2.3','mortality2.4')){

  suppressWarnings({
    dfe <-
    data.table::melt(mr, id.vars=c(1:7)) %>%
    select(species, year, vessel, channel, month, diel, iteration, event = variable, outcome = value)
  })

  head(dfe)

  dfe$event <- factor(dfe$event, levels = events)

  return(dfe)
}
