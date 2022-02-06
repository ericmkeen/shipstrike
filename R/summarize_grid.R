#' Summarize vessel grid by type, month, diel, and grid_id
#'
#' This is essentially a wrapper for a `dplyr::group_by() ... summarize()` sequence.
#'
#' @param vgrid An interpolated record of vessel position fixes,
#' as produced by `shipstrike::vessel_grid()`.
#'
#' @return A `list` with two slots:
#' \itemize{
#' \item `grid` holds a `data.frame` in which each row corresponds to a single grid cell
#' within a single month within a single diel period,
#' and the columns `speed`, `length`, `width`, and `draft` record the metrics for each vessel
#' that enters the cell, with the metric for each transit separated by an underscore (`_`).
#' The column `n` indicates the number of transits that intersected the grid cell.
#'
#' \item `params.ship` holds a `data.frame` in which each row corresponds to
#' vessel parameters during the transit of a single grid cell; there is a row for each
#' grid cell intersected across all transits, such that this `data.frame` represents
#' the spatially weighted distribution of characteristics to pass to the `encounter_simulator()`.
#' This dataset has three columns: `v.ship` (speed, in knots), `l.ship` (length, in meters), and `w.ship` (beam width, in meters).
#' Pass this slot to the `params.ship` input in `encounter_simulator()`.
#' }
#'
#' @export
#'
summarize_grid <- function(vgrid){

  head(vgrid)
  vgrid$month <- lubridate::month(vgrid$datetime)

  # Slot grid
  vsumm <-
    vgrid %>%
    dplyr::group_by(type, month, diel, grid_id) %>%
    dplyr::summarize(x = x[1],
                     y= y[1],
                     n = dplyr::n(),
                     #vid = paste(vid, collapse='_'),
                     speed = paste(round(speed,1), collapse='_'),
                     length = paste(round(length,1), collapse='_'),
                     width = paste(round(width,1), collapse='_'),
                     draft = paste(round(draft,1), collapse='_'))

  # Slot params.ship
  params.ship <- apply(vsumm, 1, function(vsummi){
    vsummi <- vsummi %>% as.character
    (speeds <- stringr::str_split(vsummi[8],'_') %>% unlist %>% as.numeric)
    (lengths <- stringr::str_split(vsummi[9],'_') %>% unlist %>% as.numeric)
    (widths <- stringr::str_split(vsummi[10],'_') %>% unlist %>% as.numeric)
    dfi <- data.frame(v.ship = speeds, l.ship = lengths, w.ship = widths)
    return(dfi)
  })
  params.ship <- bind_rows(params.ship)

  return_list <- list(grid = vsumm, params.ship = params.ship)

  return(return_list)
}
