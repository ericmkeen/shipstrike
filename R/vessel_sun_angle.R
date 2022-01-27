#' Calculate sun angle for a vessel based on `datetime`, `x`, and `y`
#'
#' @param vessels desc
#'
#' @return
#' @export
#'
vessel_sun_angle <- function(vessels){

  if(FALSE){
    vessels <- vgrid
  }

  head(vessels)
  pb <- txtProgressBar(1, nrow(vessels), style=3) # setup progress bar
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
    setTxtProgressBar(pb, i) # update progress bar
  }

  return(sun_angle)
}
