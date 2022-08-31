#' Plot raw outcomes: diels
#'
#' @param outcomes desc
#'
#' @return desc
#' @export
#' @import dplyr
#' @import ggplot2
#'
plot_outcomes_diels <- function(outcomes){

  # Rename for simplicity
  mr <- outcomes

  # Melt data
  mrm <- outcome_melt(mr)

  # Summarize data
  melti <-
    mrm %>%
    group_by(event, diel, iteration) %>%
    summarize(outcome = sum(outcome))

  ggplot(melti, aes(x=outcome, y=diel)) +
    geom_jitter(height = .35, width=.4, alpha=.1, cex=.8, color = 'darkblue') +
    xlab('Number of interactions') +
    ylab('Diel period') +
    facet_wrap(~event, scales='free_x')
}
