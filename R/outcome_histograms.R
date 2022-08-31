#' Create histograms summarizing whale-ship outcomes
#'
#' @param outcomes A `data.frame` of results from the `shipstrike::gather_outcomes()` function.
#' @param surface_var desc
#' @param collision_var desc
#' @param mortality_var desc
#' @param orientation desc
#'
#' @return desc
#' @export
#' @import ggplot2
#' @import dplyr
#' @import data.table
#'
outcome_histograms <- function(outcomes,
                               surface_var = 'surface2',
                               collision_var = 'collision2.2',
                               mortality_var = 'mortality2.2',
                               plot_all = FALSE,
                               orientation = 'vertical'){
  mr <- outcomes
  mr %>% names

  melted <- outcome_melt(mr)
  head(melted)
  melted <-
    melted %>%
    group_by(event, iteration) %>%
    summarize(outcome = sum(outcome))

  head(melted)

  # all ========================================================================

  gg_outcomes_all <- NULL
  if(plot_all){
    gg_outcomes_all <-
      ggplot2::ggplot(melted,
                      ggplot2::aes(x=outcome)) +
      ggplot2::geom_bar(stat='count', width=1, alpha=.7, fill='darkslategray') +
      ggplot2::ylab('Frequency') +
      ggplot2::xlab('Predicted outcomes') +
      ggplot2::facet_wrap(~event, scales='free') +
      ggplot2::theme_light() +
      ggplot2::theme(strip.text.x = ggplot2::element_text(size=9, color='grey95', face='bold'),
                     strip.background = ggplot2::element_rect(fill="grey60"))
  }

  # polished ===================================================================

  #surface_var <- 'surface2'
  #collision_var <- 'collision2.2'
  #mortality_var <- 'mortality2.2'
  #orientation <- 'vertical'

  head(melted)
  keeps <- c('cooccurrence', 'encounter', surface_var, collision_var, mortality_var)
  melti <- melted %>%
    dplyr::filter(as.character(event) %in% keeps) %>%
    mutate(event = as.character(event))
  nrow(melti)

  melti$event %>% unique
  melti$event[as.character(melti$event) == 'cooccurrence'] <- 'Cooccurrence'
  melti$event[as.character(melti$event) == 'encounter'] <- 'Close encounter'
  melti$event[as.character(melti$event) == surface_var] <- 'Strike-zone event'
  melti$event[as.character(melti$event) == collision_var] <- 'Collision'
  melti$event[as.character(melti$event) == mortality_var] <- 'Mortality'
  melti$event %>% as.character %>% unique
  melti$event <- factor(melti$event, levels = c('Cooccurrence', 'Close encounter', 'Strike-zone event', 'Collision', 'Mortality'))

  gg <-
    ggplot2::ggplot(melti,
                    ggplot2::aes(x=outcome)) +
    ggplot2::geom_bar(stat='count', width=1, alpha=.7, fill='darkslategray')

  if(orientation == 'vertical'){
    gg <- gg + ggplot2::facet_wrap(~event, scales='free', ncol=1)
  }
  if(orientation == 'horizontal'){
    gg <- gg + ggplot2::facet_wrap(~event, scales='free', nrow=1)
  }

  gg <-
    gg +
    ggplot2::ylab('Frequency') +
    ggplot2::xlab('Predicted outcomes') +
    ggplot2::theme_light() +
    ggplot2::theme(strip.text.x = ggplot2::element_text(size=9, color='grey95', face='bold'),
                   strip.background = ggplot2::element_rect(fill="grey60"))

  #gg

  return(list(all = gg_outcomes_all, simple = gg))
}
