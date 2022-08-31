#' Plot raw outcomes: vessels
#'
#' @param outcomes desc
#'
#' @return desc
#' @export
#' @import dplyr
#' @import ggplot2
#'
plot_outcomes_vessels <- function(outcomes){

  # Rename for simplicity
  mr <- outcomes

  # Melt data
  mrm <- outcome_melt(mr)

  # Summarize data
  melti <-
    mrm %>%
    group_by(event, vessel, iteration) %>%
    summarize(outcome = sum(outcome))


  ggplot(melti, aes(x=outcome, y=vessel)) +
    geom_jitter(height = .35, width=.4, alpha=.05, cex=.3, color = 'darkblue') +
    xlab('Number of interactions') +
    ylab(NULL) +
    facet_wrap(~event, scales='free_x')
}
