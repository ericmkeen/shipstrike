#' Create a spatial grid
#'
#' @param xlims A two-element numeric vector for the left and right limits of your grid,
#' in decimal degrees of longitude in which W coordinates are negative.
#' @param ylims A two-element numeric vector for the bottom and top limits of your grid,
#' in decimal degrees of latitude.
#' @param grid_int The target interval (in north-south km) represented by each grid cell.
#' This interval will be translated to degrees latitude and longitude.
#' If your target is a square km, you may need to adjust this slightly to account for
#' the smaller distance represented by a degree of longitude at higher latitudes.
#'
#' @return A `data.frame` with a row for every grid cell and the following columns:
#' \itemize{
#' \item `y1` Bottom (southern) latitude boundary of the grid cell
#' \item `y2` Top (northern) latitude boundary of the grid cell
#' \item `y` Center latitude of the grid cell
#' \item `x1` Left (western) longitude of the grid cell
#' \item `x2` Right (easter) longitude of the grid cell
#' \item `x` Center longitude of the grid cell
#' \item `km2` Spatial area of the grid cell, in square km
#' \item `id` A unique identifier for each grid cell (`1:nrow(grid)`)
#' }
#' @export
#'
make_grid <- function(xlims,
                      ylims,
                      grid_int = 1){
  (y_int <- 1 / (110.57 / grid_int)) # latitude interval
  (ys <- seq(ylims[1],ylims[2],by=y_int))
  (xs <- seq(xlims[1], xlims[2], by=y_int))

  lats <- data.frame(y1=ys[1:(length(ys)-1)], y2=ys[2:length(ys)])
  lats$y <- apply(lats, 1, mean)
  head(lats)
  lons <- data.frame(x1=xs[1:(length(xs)-1)], x2=xs[2:length(xs)])
  lons$x <- apply(lons, 1, mean)
  head(lons)

  pb <- txtProgressBar(1, nrow(lats), style=3) # setup progress bar
  grid_cells <- data.frame()
  i=1
  for(i in 1:nrow(lats)){
    lati <- lats[i,]
    gridi <- data.frame(lati, lons)
    head(gridi)
    gridi$km2 <- apply(gridi, 1, function(x){
      d <- cbind(c(x[4], x[5], x[5], x[4], x[4]),
                 c(x[1], x[1], x[2], x[2], x[1]))
      geosphere::areaPolygon(d) *10^(-6)
    })
    grid_cells <- rbind(grid_cells, gridi)
    setTxtProgressBar(pb, i) # update progress bar
    if(i == nrow(lats)){message('\n')}
  }

  nrow(grid_cells)
  grid_cells$id <- 1:nrow(grid_cells)
  head(grid_cells)
  #hist(grid_cells$km2)

  return(grid_cells)
}
