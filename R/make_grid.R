#' Create a spatial grid
#'
#' @param xlims desc
#' @param ylims desc
#' @param grid_int adjust grid_int (km) to get mean grid km2 of 1.00 km2
#'
#' @return
#' @export
#'
make_grid <- function(xlims,
                      ylims,
                      grid_int){
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
