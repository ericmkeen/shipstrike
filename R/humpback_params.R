#' Humpback whale parameters to use in ship-strike analysis
#'
#' A list of parameters used for humpback whales in the analyses within *Keen et al. (2023)*.
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
humpback_params <- function(length_min = 8.25,
                       length_max = 15.5,
                       length_mean = 11.85,
                       length_sd = 1.32,
                       width_factor = 0.330,
                       speed_day_min = 0.13888,
                       speed_day_max = 1.166,
                       speed_day_mean = 0.7111,
                       speed_day_sd = 0.20833,
                       speed_night_min = 0.22035,
                       speed_night_max = 1.85,
                       speed_night_mean = 1.128,
                       speed_night_sd = 0.2247,
                       delta_day_mean = 0.8087532,
                       delta_day_sd = 1.332026,
                       delta_night_mean = 1.494988,
                       delta_night_sd = 1.386166){

  params <- as.list(environment())
  return(params)
}
