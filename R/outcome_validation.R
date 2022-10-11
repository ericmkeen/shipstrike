#' Outcome validation simulation
#'
#' This function replicates the validation exercise from Keen et al. (2023)
#' to determine what our strike detection rate would need to be in order for
#' our results to be plausible given that we have never observed a strike in our study area.
#'
#'
#' @param outcomes A `data.frame` of results from the `shipstrike::gather_outcomes()` function.
#' @param observations The number of observed strike events during observation period.
#' @param scaling_factor Deprecated -- keep at 1.
#' @param years Years of observation.
#' @param months Months of effort during each year.
#' @param iterations Number of iterations to use for randomization routine.
#' @param toplot Produce plots?
#'
#' @return A named `list` including `ggplot2` objects, as well as output printed to console.
#' @export
#' @import dplyr
#' @import ggplot2
#'
outcome_validation <- function(outcomes,
                               eventcol = 'mortality2.2',
                               observations = 0,
                               scaling_factor = 1,
                               years = 10,
                               months = 6:9,
                               iterations = 1000,
                               toplot=FALSE){


  if(FALSE){
    outcomes <- readRDS('../fw/ais_2015/outcomes.RData')
    eventcol <- 'mortality2.2'
    observations = 0
    scaling_factor = 1
    years = 10
    months = 6:9
    iterations = 10000
    toplot = TRUE

    results <- outcome_validation(outcomes)
  }

  # Prepare posterior ==========================================================
  message('Melting outcomes & prepping the posterior ...')

  melted <- outcome_melt(outcomes)
  head(melted)

  suppressMessages({
    posterior <-
    melted %>%
    filter(event == eventcol) %>%
    filter(month %in% months) %>%
    group_by(event, iteration) %>%
    summarize(outcome = sum(outcome)) %>%
    pull(outcome)
  })

  #hist(posterior)
  posterior

  # Probability of observations ================================================
  message('Determining the probability of your observations ...')

  iteration <- rep(1:iterations, each=years)
  year <- rep(1:years, times=iterations)
  strikes <- sample(posterior, size=(iterations*years), replace=TRUE)
  sims <- data.frame(iteration, year, strikes)
  sim_decades <-
    sims %>%
    dplyr::group_by(iteration) %>%
    dplyr::summarize(strikes = sum(strikes)) %>%
    dplyr::mutate(strikes_observed = strikes * scaling_factor)

    gg_L <-
      ggplot2::ggplot(data = sim_decades,
                   mapping = ggplot2::aes(x=strikes_observed)) +
      ggplot2::geom_bar(stat='count', fill='darkblue', alpha=.3, width=1) +
      ggplot2::geom_vline(xintercept = observations, lty=2, col='firebrick', lwd=1) +
      scale_x_continuous(breaks = seq(0, max(sim_decades$strikes_observed) + 1, by=2)) +
      xlab('Strikes during monitoring window (simulated)') + ylab('Count') +
      theme_light()

    if(toplot){ print(gg_L) }

  # Likelihood of observation
  L <- length(which(sim_decades$strikes_observed <= observations)) / nrow(sim_decades)
  message('Likelihood of your observation, assuming perfect detection = ', L)


  # Finding SDR ================================================================

  message('Finding the strike detection rate (SDR) that would make your observations plausible ...')

  sdrs <- seq(0.01, 1, by=0.005)

  sdr_test <-
    data.frame(SDR = rep(sdrs, each=nrow(sim_decades)),
               actual = rep(sim_decades$strikes_observed, times=length(sdrs)))

  sdr_test$observed <-
    apply(sdr_test, 1, function(x){
    rnums <- runif(x[2], 0, 1)
    observed <- length(which(rnums < x[1]))
    return(observed)
  })

  sdr_results <-
    sdr_test %>%
    group_by(SDR) %>%
    summarize(L = length(which(observed == observations))/ n())

  (sdr.05 <- sdr_results$SDR[which.min(abs(0.05 - sdr_results$L))])
  (sdr.1 <- sdr_results$SDR[which.min(abs(0.1 - sdr_results$L))])
  (sdr.2 <- sdr_results$SDR[which.min(abs(0.2 - sdr_results$L))])
  (sdr.55 <- sdr_results$SDR[which.min(abs(0.55 - sdr_results$L))])


  # Plot it ====================================================================

  message('preparing L ~ SDR plot ...')

  gg_sdr <-
    ggplot(sdr_results, aes(x=SDR, y=L)) +
    geom_path(lwd=1) +
    geom_hline(yintercept = 0.05, lty=2, lwd=.5, col='darkblue', alpha=.4) +
    geom_hline(yintercept = 0.1, lty=2, lwd=.5, col='darkblue', alpha=.4) +
    geom_hline(yintercept = 0.2, lty=2, lwd=.5, col='darkblue', alpha=.4) +
    geom_hline(yintercept = 0.55, lty=2, lwd=.5, col='darkblue', alpha=.4) +
    geom_point(mapping = aes(x=sdr.05, y=0.05), size=3, col='firebrick', alpha=.3, pch=1) +
    geom_point(mapping = aes(x=sdr.1, y=0.1), size=3, col='firebrick', alpha=.3, pch=1) +
    geom_point(mapping = aes(x=sdr.2, y=0.2), size=3, col='firebrick', alpha=.3, pch=1) +
    geom_point(mapping = aes(x=sdr.55, y=0.55), size=3, col='firebrick', alpha=.3, pch=1) +
    xlab('Strike detection rate') +
    ylab('Prob. of no mortality observed') +
    scale_y_continuous(breaks=seq(0, 1, by=0.1), limits = c(0,1)) +
    theme_light()

  if(toplot){ print(gg_sdr) }

  message('--- SDR needed for P(Observation) of 0.05 = ', sdr.05)
  message('--- SDR needed for P(Observation) of 0.10 = ', sdr.1)
  message('--- SDR needed for P(Observation) of 0.20 = ', sdr.2)
  message('--- SDR needed for P(Observation) of 0.55 = ', sdr.55)

  returni <- list(L = L,
                  sim = sim_decades,
                  L_plot = gg_L,
                  SDR.05 = sdr.05,
                  SDR.10 = sdr.1,
                  SDR.20 = sdr.2,
                  SDR.55 = sdr.55,
                  SDR = sdr_results,
                  SDR_plot = gg_sdr)

  return(returni)
}
