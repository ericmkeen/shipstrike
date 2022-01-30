#' Get seafloor features at a spatial point
#'
#' @param lon desc
#' @param lat desc
#' @param seafloor desc
#'
#' @return
#' @export
#'
get_seafloor <- function(lon,lat,seafloor){
  #lon <- -129.4380
  #lat <- 53.17300
  head(seafloor)
  nrow(seafloor)

  z <- zmin <- zmax <- zsd <- NA

  # get seafloors within a km
  seas <- seafloor %>% dplyr::filter(x >= lon - 0.009009,
                                     x <= lon + 0.009009,
                                     y >= lat - 0.009009,
                                     y <= lat + 0.009009)
  # Of these, find range & sd
  (zmin <- abs(max(seas$layer)))
  (zmax <- abs(min(seas$layer)))
  (zsd <- abs(sd(seas$layer)))

  # Of these, find closest depth reading to lat/long
  seas$km <- apply(seas,1,function(seasi){
    swfscMisc::distance(lat1 = lat,
                        lon1 = lon,
                        lat2 = seasi[2],
                        lon2 = seasi[1],
                        units='km')
  })
  (km_min <- min(seas$km))
  matchi <- NULL
  if(km_min < .25){matchi <- which.min(seas$km)}
  if(!is.null(matchi)){z <- abs(seas$layer[matchi])}
  zf <- data.frame(z, zmin, zmax, zsd)
  return(zf)
}
