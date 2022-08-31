#' Tabular summary of ship strike outcomes
#'
#' @param outcomes A `data.frame` of results from the `shipstrike::gather_outcomes()` function.
#'
#' @return
#' @export
#'
outcome_table <- function(outcomes){
  mr <- outcomes

  melted <- outcome_melt(mr)
  head(melted)

  grand_table <-
    melted %>%
    group_by(event, iteration) %>%
    summarize(outcome = sum(outcome)) %>%
    group_by(event) %>%
    summarize(mean = round(mean(outcome),2),
              median = median(outcome),
              q5 = quantile(outcome, .05),
              q95 = quantile(outcome,.95),
              q20 = quantile(outcome,.2))

  grand_table
  return(grand_table)
}
