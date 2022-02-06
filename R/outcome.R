#' Core function for whale-ship collision outcome
#'
#' @param p_whale desc
#' @param p_encounter desc
#' @param p_surface desc
#' @param p_avoidance desc
#' @param p_lethality desc
#'
#' @return
#' @export
#'
outcome <- function(p_whale,
                    p_encounter,
                    p_surface,
                    p_avoidance,
                    p_lethality,
                    iterations=1000){

  # Core function: results for one value for each parameter, for a single cell

  if(FALSE){# debugging
    p_whale = 0.5; p_encounter = 0.5; p_surface = 0.5; p_avoidance = 0.10; p_lethality = 0.50
    iterations = 1000
  }

  # Co-occurrences
  ps = sample(p_whale, iterations, TRUE)
  runifs <- runif(iterations, 0, 1)
  verdicts <- runifs < ps
  verdicts[! verdicts] <- 0
  verdicts[verdicts] <- 1
  dfi <- data.frame(cooccurrence = verdicts %>% as.numeric)
  apply(dfi, 2, sum)

  # Close encounters
  ps = sample(p_encounter, iterations, TRUE)
  runifs <- runif(iterations, 0, 1)
  verdicts <- runifs < ps & dfi$cooccurrence == 1
  verdicts[! verdicts] <- 0
  verdicts[verdicts] <- 1
  dfi$encounter <- verdicts %>% as.numeric
  apply(dfi, 2, sum)

  # Surface overlap
  ps = sample(p_surface, iterations, TRUE)
  runifs <- runif(iterations, 0, 1)
  verdicts <- runifs < ps & dfi$encounter == 1
  verdicts[! verdicts] <- 0
  verdicts[verdicts] <- 1
  dfi$surface <- verdicts %>% as.numeric
  apply(dfi, 2, sum)

  # Avoidance
  ps = sample((1 - p_avoidance), iterations, TRUE)
  runifs <- runif(iterations, 0, 1)
  verdicts <- runifs < ps & dfi$surface == 1
  verdicts[! verdicts] <- 0
  verdicts[verdicts] <- 1
  dfi$collision <- verdicts %>% as.numeric
  apply(dfi, 2, sum)

  # Lethality
  ps = sample(p_lethality, iterations, TRUE)
  runifs <- runif(iterations, 0, 1)
  verdicts <- runifs < ps & dfi$collision == 1
  verdicts[! verdicts] <- 0
  verdicts[verdicts] <- 1
  dfi$mortality <- verdicts %>% as.numeric
  apply(dfi, 2, sum)

  # Get stochastic values
  #(is_whale <- ifelse(runif(1,0,1) < p_whale, 1, 0))
  #(is_encounter <- ifelse(runif(1,0,1) < p_encounter, 1, 0))
  #(is_surface <- ifelse(runif(1,0,1) < p_surface, 1, 0))
  #(is_avoidance <- ifelse(runif(1,0,1) < p_avoidance, 1, 0))
  #(is_lethal <- ifelse(runif(1,0,1) < p_lethality, 1, 0))

  # Get outcomes
  #cooccurrence <- is_whale
  #encounter <- cooccurrence * is_encounter
  #imminent_collision <- encounter * is_surface
  #collision <- ifelse(is_avoidance, 0, imminent_collision)
  #lethality <- collision * is_lethal

  dfi %>% head
  result <- apply(dfi, 2, sum) %>% as.data.frame(col.names=names(dfi)) %>% t
  result <- result %>% as.data.frame


  #(result <- data.frame(cooccurrence, encounter, imminent_collision, collision, lethality))
  return(result)
}
