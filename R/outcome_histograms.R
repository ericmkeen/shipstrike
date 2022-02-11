#' Create histograms summarizing whale-ship outcomes
#'
#' @param results A `data.frame` of results from the `shipstrike::outcome()` function.
#'
#' @return
#' @export
#'
outcome_histograms <- function(results){

  head(results)
  nrow(results)

  df1 <- data.frame(Outcome = 'Co-occurrence', events = results$cooccurrence)
  df2 <- data.frame(Outcome = 'Close encounter', events = results$encounter)
  df3 <- data.frame(Outcome = 'Surface overlap', events = results$surface)
  df4 <- data.frame(Outcome = 'Collision', events = results$collision)
  df5 <- data.frame(Outcome = 'Mortality', events = results$mortality)
  df <- rbind(df1, df2, df3, df4, df5)
  df$facet = factor(df$Outcome, levels = c("Co-occurrence",
                                        "Close encounter",
                                        "Surface overlap",
                                        "Collision",
                                        "Mortality"))

  ggplot2::ggplot(df,
                  ggplot2::aes(x=events)) +
    ggplot2::geom_bar(stat='count', width=1, alpha=.7, fill='darkslategray') +
    ggplot2::ylab('Frequency') +
    ggplot2::xlab('Predicted events') +
    ggplot2::facet_wrap(~facet, scales='free', nrow=3, ncol=2) +
    ggplot2::theme_light() +
    ggplot2::theme(strip.text.x = ggplot2::element_text(size=9, color='grey95', face='bold'),
          strip.background = ggplot2::element_rect(fill="grey60"))

}
