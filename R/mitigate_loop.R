#' Loop through months to find most efficacious mitigation window
#'
#' To test the effects of seasonally displacing ship traffic
#' (i.e., rescheduling transits from a given month into other months)
#' *or* seasonally cancelling ship traffic
#' (i.e., a moratorium), you can use the `shipstrike` function `mitigate_loop()`,
#' which applies the displacement window to each candidate month in a loop.
#' This allows us to determine which month would be the most efficacious target for mitigation.
#'
#' @param outcomes The result of `outcome_predict()`.
#' @param mitigation_duration Number indicating the number of months to use as a mitigation period.
#' Keen et al. (2023) tests durations of 1, 2, and 3 months.
#' @param reschedule Boolean, with default `FALSE`, indicating whether traffic during the mitigation window
#' should be rescheduled (`reschedule == TRUE`) or cancelled (`FALSE)`.
#' @param vessels A character vector indicating for which vessel types this mitigation action applies.
#'
#' @return See the `shipstrike` package vignette & Keen et al. (2023) for details.
#' @export
#'
mitigate_loop <- function(outcomes,
                          mitigation_duration = 1,
                          reschedule = FALSE,
                          vessels = c('Tanker > 100m', 'Cargo > 100m', 'Passenger > 100m',
                                     'LNG Canada tanker in-heel', 'LNG Canada tanker in-product',
                                     'LNG Canada tug in-heel', "LNG Canada tug in-product",
                                     "Cedar LNG tanker in-heel", "Cedar LNG tanker in-product",
                                     "Cedar LNG tug in-heel", "Cedar LNG tug in-product")
                          ){

  #mitigation_duration = 1
  #reschedule = TRUE

  # Loop through month candidates for the moratorium
  mitigated_hist <- outcomes %>% group_by(iteration) %>%
    summarize(outcome = sum(mortality2.2)) %>% mutate(test = 'Baseline') %>% select(test, outcome)
  (mitigated_outcomes <- outcome_table(outcomes) %>% mutate(test = 'Baseline'))
  mitigated_chances <- outcome_chances(outcomes)$at_least %>% mutate(test = 'Baseline')
  i=7
  for(i in 1:12){

    # Prepare time period
    (monthi <- i:(i+ mitigation_duration-1))
    if(any(monthi>12)){
      monthi[monthi > 12] <- monthi[monthi > 12] - 12
    }
    monthi

    # Title
    (start_month <- stringr::str_pad(i, 2, 'left', '0'))
    (end_month <- stringr::str_pad(monthi[length(monthi)], 2, 'left', '0'))
    if(mitigation_duration > 1){
      (titi <- paste0('Months ', start_month,'-',end_month))
    }else{
      (titi <- paste0('Month ', start_month))
    }
    titi
    message(titi)

    # Mitigate
    mitigation_filters <- list(list(month = monthi,
                                    vessel = vessels,
                                    displace = reschedule))
    mitigation_filters
    # before
    outcomes$mortality2.2 %>% sum
    # mitigate
    mitigated <- mitigate(outcomes, mitigation_filters)
    #after
    mitigated$mortality2.2 %>% sum

    # Histograms
    hist_mi <- mitigated %>% group_by(iteration) %>%
      summarize(outcome = sum(mortality2.2)) %>%
      mutate(test = titi) %>% select(test, outcome)
    mitigated_hist <- rbind(mitigated_hist, hist_mi)

    # Outcomes
    out_mi <- outcome_table(mitigated) %>%
    mutate(test = titi)
    out_mi
    mitigated_outcomes <- rbind(mitigated_outcomes, out_mi)

    # Chances
    chances_mi <- outcome_chances(mitigated)
    chances_mi <- chances_mi$at_least %>%
      mutate(test = titi)
    chances_mi
    mitigated_chances <- rbind(mitigated_chances, chances_mi)
  }

  mitigation_results <- list(outcomes = mitigated_outcomes,
                             chances = mitigated_chances,
                             hist = mitigated_hist)

  return(mitigation_results)
}
