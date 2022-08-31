#' AIS table
#'
#' @param ais  desc
#'
#' @return
#' @export
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
