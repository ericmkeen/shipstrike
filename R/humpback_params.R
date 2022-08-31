#' Humpback whale parameters
#'
#' @param length_min Whale dimensions will be drawn from
#' a truncated normal distribution (source: TBD)
#' @param length_max desc
#' @param length_mean desc
#' @param length_sd desc
#' @param width_factor will be used to scale length to find width
#' @param speed_day_min Whale speed will be drawn from
#' a truncated normal distribution (source: TBD)
#' @param speed_day_max desc
#' @param speed_day_mean desc
#' @param speed_day_sd desc
#' @param speed_night_min desc
#' @param speed_night_max desc
#' @param speed_night_mean desc
#' @param speed_night_sd desc
#' @param delta_day_mean Whale track variability defined as the
#' standard deviation of changes in whale course every 60 seconds;
#' values will be drawn from a normal distribution then reverse-log-transformed
#' (source: using fin whale data -- Hendricks et al. 2021)
#' @param delta_day_sd desc
#' @param delta_night_mean desc
#' @param delta_night_sd desc
#'
#' @return
#' @export
#'
humpback_params <- function(length_min = 8.5344,
                       length_max = 15.448,
                       length_mean = 12.1158,
                       length_sd = 1.524,
                       width_factor = 0.23778,
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
