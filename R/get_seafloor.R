#' Get seafloor features at a spatial point
#'
#' @param lon The longitude (decimal degrees; West coordinates are negative) of interest.
#' This can be a single value or a vector of values.
#' @param lat The latitude (decimal degrees; South coordinates are negative).
#' This must be the same length as `lon`.
#' @param seafloor A `data.frame` with seafloor depths; required columns are
#' `x` (longitude, decimal degrees), `y` (latitude), and `layer` (with the seafloor depth).
#' @param lat_range The range around the provided coordinate, expressed as a numeric representing
#' degrees latitude, used to summarize seafloor features. The default represents 1 km north-south.
#'
#' @return A `dataframe`, with the number of rows equal to the length of `lon`, with the
#' following columns:
#' \itemize{
#' \item `z` Seafloor depth nearest to the coordinate.
#' \item `zmin` Minimum seafloor depth within `lat_range` of the coordinate.
#' \item `zmax` Maximum seafloor depth.
#' \item `zsd` Standard deviation of seafloor depth within `lat_range`.
#' }
#' @export
#'
get_seafloor <- function(lon,
                         lat,
                         seafloor,
                         lat_range = 0.009009){
  #lon <- -129.4380
  #lat <- 53.17300
  head(seafloor)
  nrow(seafloor)

  z <- zmin <- zmax <- zsd <- NA

  # get seafloors within a km
  seas <- seafloor %>% dplyr::filter(x >= lon - lat_range,
                                     x <= lon + lat_range,
                                     y >= lat - lat_range,
                                     y <= lat + lat_range)
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
