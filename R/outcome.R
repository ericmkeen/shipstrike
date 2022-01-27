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
                    p_lethality){

  # Core function: results for one value for each parameter, for a single cell

  if(FALSE){# debugging
    p_whale = 0.5; p_encounter = 0.5; p_surface = 0.5; p_avoidance = 0.10; p_lethality = 0.50
  }

  # Get stochastic values
  (is_whale <- ifelse(runif(1,0,1) < p_whale, 1, 0))
  (is_encounter <- ifelse(runif(1,0,1) < p_encounter, 1, 0))
  (is_surface <- ifelse(runif(1,0,1) < p_surface, 1, 0))
  (is_avoidance <- ifelse(runif(1,0,1) < p_avoidance, 1, 0))
  (is_lethal <- ifelse(runif(1,0,1) < p_lethality, 1, 0))

  # Get outcomes
  cooccurrence <- is_whale
  encounter <- cooccurrence * is_encounter
  imminent_collision <- encounter * is_surface
  collision <- ifelse(is_avoidance, 0, imminent_collision)
  lethality <- collision * is_lethal

  (result <- data.frame(cooccurrence, encounter, imminent_collision, collision, lethality))
  return(result)
}
