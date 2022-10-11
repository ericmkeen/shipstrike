#' Produce a table of summary statistics for AIS traffic.
#'
#' @param ais  A `data.frame` of AIS traffic records for a single year. Each row is a position fix. Required fields:
#' \itemize{
#' \item `vid` = Unique vessel identifier (numeric)
#' \item `type` = Vessel type (character string)
#' \item `speed` = Vessel speed, in knots (numeric)
#' \item `length` = Vessel length, in meters (numeric)
#' \item `width` = Vessel beam width, in meters (numeric)
#' \item `draft` = Vessel beam width, in meters (numeric)
#' \item `datetime` = Datetime in UTC, with format `yyyy-mm-dd hh:mm:ss`
#' \item `x` = Longitude, decimal degrees (Western degrees negative)
#' \item `y` = Latitude, decimal degrees (Southern degrees negative)
#' }
#'
#' @return A `data.frame` of summary statistics for each vessel type.
#'
#' @export
#' @examples
#' data(ais_2019)
#' ais_table(ais_2019)
#'
ais_table <- function(ais){
  ais %>%
    group_by(vid) %>%
    mutate(dates = c(length(unique(lubridate::yday(datetime))),rep(0,times=(n()-1)))) %>%
    ungroup() %>%
    group_by(type) %>%
    summarize(vids = unique(vid) %>% length,
              fixes = n(),
              transits = sum(dates),
              transit_rate = round(transits / 365, 2),
              dates_present = lubridate::yday(datetime) %>% unique %>% length,
              present_rate = round(dates_present / 365, 2),
              speed_mn = mean(speed, na.rm=TRUE) %>% round(1),
              speed_sd = sd(speed, na.rm=TRUE) %>% round(1),
              speed_max = max(speed, na.rm=TRUE) %>% round(1),
              length_mn = mean(length, na.rm=TRUE) %>% round(),
              length_sd = sd(length, na.rm=TRUE) %>% round(),
              length_min = min(length, na.rm=TRUE) %>% round(),
              length_max = max(length, na.rm=TRUE) %>% round(),
              beam_mn = mean(width, na.rm=TRUE) %>% round(),
              beam_sd = sd(width, na.rm=TRUE) %>% round(),
              beam_min = min(width, na.rm=TRUE) %>% round(),
              beam_max = max(width, na.rm=TRUE) %>% round(),
              draft_mn = mean(draft, na.rm=TRUE) %>% round(1),
              draft_sd = sd(draft, na.rm=TRUE) %>% round(1),
              draft_min = min(draft, na.rm=TRUE) %>% round(1),
              draft_max = max(draft, na.rm=TRUE) %>% round(1))
}
