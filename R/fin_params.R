#' Fin whale parameters to use in ship-strike analysis
#'
#' A list of parameters used for fin whales in the analyses within *Keen et al. (2023)*.
#' These parameters include all the details needed to estimate close-encounter rates and
#' predictions of whale-vessel interaction rates and collisions. You can use these same values
#' to get started in setting up your analysis,
#' then use this function to modify the values to fit your specific study.
#'
#' The sources and rationale for these values are explaiend in *Keen et al. (2023)*.
#'
#' @param length_min Minimum whale length (meters) used to define a truncated normal distribution.
#' @param length_max Maximum whale length (meters) used to define a truncated normal distribution.
#' @param length_mean Mean whale length (meters).
#' @param length_sd Standard deviation in whale length (meters).
#' @param width_factor The scaling factor used to estimate whale fluke width based on length.
#' For exampl, a `width_factor` of 0.1 means a 20m whale has a fluke width of 2 meters.
#' @param speed_day_min Minimum daytime whale speed (meters/second)
#' used to define a truncated normal distribution.
#' @param speed_day_max Maximum daytime whale speed (meters/second)
#' used to define a truncated normal distribution.
#' @param speed_day_mean Mean daytime whale speed (meters/second)
#' @param speed_day_sd Standard deviation of daytime whale speed (meters/second)
#' @param speed_night_min Minimum nighttime whale speed (meters/second)
#' used to define a truncated normal distribution.
#' @param speed_night_max Maximum nighttime whale speed (meters/second)
#' used to define a truncated normal distribution.
#' @param speed_night_mean Mean nighttime whale speed (meters/second)
#' @param speed_night_sd Standard deviation of nighttime whale speed (meters/second)
#' @param delta_day_mean Mean of daytime whale track variability, defined as the
#' standard deviation of changes in whale course (log-transformed) every 60 seconds;
#' values will be drawn from a normal distribution with mean 0, then reverse-log-transformed.
#' @param delta_day_sd Standard deviation of track variability.
#' @param delta_night_mean Mean of nighttime whale track variability.
#' @param delta_night_sd Standard deviation of nighttime whale track variability.
#'
#' @return A named list of these inputs, without any changes at all. Pass this list to
#' `shipstrike::encounter_rate()` or `shipstrike::outcome_predict()`.
#'
#' @export
#'
fin_params <- function(length_min = 10,
                       length_max = 26,
                       length_mean = 20,
                       length_sd = 1.65,
                       width_factor = 0.2074,
                       speed_day_min = 0.27,
                       speed_day_max = 2.22,
                       speed_day_mean = 1.140301,
                       speed_day_sd = 0.4375298,
                       speed_night_min = 0.55,
                       speed_night_max = 3.04,
                       speed_night_mean = 1.809226,
                       speed_night_sd = 0.5718535,
                       delta_day_mean = 0.8087532,
                       delta_day_sd = 1.332026,
                       delta_night_mean = 1.494988,
                       delta_night_sd = 1.386166){

  params <- as.list(environment())
  return(params)
}
