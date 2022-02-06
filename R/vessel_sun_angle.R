#' Calculate sun angle for a vessel based on `datetime`, `x`, and `y`
#'
#' This is essentially a convenient wrapper for `suncalc::getSunlightPosition`.
#'
#' @param vessels A `date.frame` that has at least the following following columns:
#' \itemize{
#' \item `datetime` = Datetime in UTC, with format `yyyy-mm-dd hh:mm:ss`.
#' \item `x` = Longitude, decimal degrees (Western degrees negative).
#' \item `y` = Latitude, decimal degrees (Southern degrees negative).
#' }
#' @param verbose A Boolean; if `TRUE`, updates will be printed to the Console.
#'
#' @return A vector, the same length as the number of rows in `vessels`, with the
#' local vertical angle of the sun at each row.
#'
#' @export
#'
vessel_sun_angle <- function(vessels, verbose=FALSE){

  if(FALSE){
    vessels <- vgrid
  }

  head(vessels)
  if(verbose){pb <- txtProgressBar(1, nrow(vessels), style=3)} # setup progress bar
  sun_angle <- c()
  i=1
  for(i in 1:nrow(vessels)){
    (vessi <- vessels[i,])
    vessi$datetime
    rads <- suncalc::getSunlightPosition(date = vessi$datetime,
                                 lat = vessi$y,
                                 lon = vessi$x)$altitude
    degs <- rads * (180/pi)
    sun_angle[i] <- degs
    if(verbose){setTxtProgressBar(pb, i)} # update progress bar
  }
  message('')

  return(sun_angle)
}
