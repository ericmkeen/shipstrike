#' Plot raw outcomes: monthly
#'
#' @param outcomes desc
#'
#' @return desc
#' @export
#' @import dplyr
#' @import ggplot2
#'
plot_outcomes_months <- function(outcomes){

  # Rename for simplicity
  mr <- outcomes

  # Melt data
  mrm <- outcome_melt(mr)

  # Summarize data
  melti <-
    mrm %>%
    group_by(event, month, iteration) %>%
    summarize(outcome = sum(outcome))


  ggplot(melti, aes(x=outcome, y=factor(month))) +
    geom_jitter(height = .35, width=.4, alpha=.05, cex=.3, color = 'darkblue') +
    xlab('Number of interactions') +
    ylab('Month') +
    facet_wrap(~event, scales='free_x')
}
