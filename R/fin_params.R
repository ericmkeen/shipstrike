#' Fin whale parameters
#'
#' @param length_min Whale dimensions will be drawn from
#' a truncated normal distribution (source: Keen et al 2021)
#' @param length_max desc
#' @param length_mean desc
#' @param length_sd desc
#' @param width_factor will be used to scale length to find width
#' @param speed_day_min Whale speed will be drawn from
#' a truncated normal distribution (source: Hendricks et al 2021)
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
#' (source: Hendricks et al. 2021)
#' @param delta_day_sd desc
#' @param delta_night_mean desc
#' @param delta_night_sd desc
#'
#' @return
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
