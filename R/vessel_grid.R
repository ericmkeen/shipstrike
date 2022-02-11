#' Interpolate vessel fixes and associate with grid cells
#'
#' This function determines each spatial grid cell intersected by each vessel transit.
#' To do so, it interpolates position fixes to ensure that there is one at least every
#' 0.5 km.
#'
#' @param grids A spatial grid of cells, as produced by `shipstrike::make_grid()` (see documentation)
#' (or at least matching the format thereof).
#' @param vessels  A `data.frame` of position fixes, with the following columns
#' (note that the output from `shipstrike::simulate_vessel()` meets these criteria):
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
#' \item `km`= The distance traveled (km) in this interpolated segment
#' \item `year` = Numeric year
#' \item `month` = Numeric month (1-12)
#' \item `yday` = Numerical day of year
#' }
#' @param toplot A Boolean; if `TRUE`, progress plots will be shown.
#' @param verbose A Boolean; if `TRUE`, updates will be printed to the Console.
#'
#' @return A `data.frame` of interpolated vessel position fixes (can be quite large)
#' and the grid cell id associated with each position fix.
#'
#' @export
#'
vessel_grid <- function(grids,
                        vessels,
                        toplot = FALSE,
                        verbose=TRUE){
  #toplot = TRUE
  #vessels <- ais
  head(grids)
  head(vessels)

  # Stage result
  vgrids <- data.frame()

  # Loop through each unique vessel type
  (vtypes <- vessels$type %>%  unique())
  vtypi <- 1
  for(vtypi in 1:length(vtypes)){
    (vtype <- vtypes[vtypi])
    vesstype <- vessels %>% dplyr::filter(type == vtype)
    nrow(vesstype)
    if(verbose){message('Vessel type ',vtype,' :: Interpolating fixes & determining grid cell associations ...')}

    if(toplot){
      plot(x=grids$x, y=grids$y, cex=.1, pch=16)
    }

    # Loop through each unique vessel in this type
    (vids <- vesstype$vid %>% unique())
    if(length(vids)>1 & verbose==TRUE){pb <- txtProgressBar(1, length(vids), style=3)} # setup progress bar
    vi=1
    for(vi in 1:length(vids)){
      (vidi <- vids[vi])
      (vessi <- vesstype %>% dplyr::filter(vid==vidi))

      if(! 'year' %in% names(vessi)){vessi$year <- lubridate::year(vessi$datetime)}
      if(! 'yday' %in% names(vessi)){vessi$yday <- lubridate::yday(vessi$datetime)}

      vessi %>% head

      # Loop through each year for this vid
      (years <- vessi$year %>% unique)
      yeari <- 2022
      for(yeari in years){
        (vessy <- vessi %>% dplyr::filter(vessi$year == yeari))

        # Loop through each yday for this vid
        (ydays <- vessy$yday %>% unique)
        ydayi <- ydays[1]
        for(ydayi in ydays){
          (xy <- vessy %>% dplyr::filter(yday == ydayi))

          if(nrow(xy)>= 2){
            if(! 'km' %in% names(xy)){
              pos <- data.frame(lat1 = xy$y[1:length(xy$y)-1],
                                lon1 = xy$x[1:length(xy$x)-1],
                                lat2 = xy$y[2:length(xy$y)],
                                lon2 = xy$x[2:length(xy$x)])
              d <- apply(pos,1,function(x){swfscDAS::distance_greatcircle(x[1], x[2], x[3], x[4])})
              xy$km <- c(d,mean(d))
            }
            #xy$km

            # Interpolate ==============================================================

            xyint <- data.frame()
            xy
            i=2
            for(i in 2:nrow(xy)){
              (xy2 <- xy[i,] %>% dplyr::select(datetime:km))
              (xy1 <- xy[(i-1),] %>% dplyr::select(datetime:km))
              (kmi <- xy$km[i-1])
              (ints <- ceiling(kmi) * 2) # number of points to interpolate
              xyi <- xy1
              if(ints > 1){
                xy1 ; xy2
                if(xy1$x == xy2$x){xy2$x <- xy2$x + 0.0000001}
                if(xy1$y == xy2$y){xy2$y <- xy2$y + 0.0000001}
                (xyi <- stats::approx(x=c(xy1$x,xy2$x), y=c(xy1$y,xy2$y), n=ints) %>% data.frame)
                xyi$datetime  <- seq(xy1$datetime, xy2$datetime, length=ints)
                xyi$km <- (kmi / ints) %>% as.numeric
              }
              xyi
              xyi <-
                xyi %>%
                dplyr::mutate(vid = xy$vid[i-1],
                              type = xy$type[i-1],
                              speed = xy$speed[i-1],
                              length = xy$length[i-1],
                              width = xy$width[i-1],
                              draft = xy$draft[i-1]) %>%
                dplyr::select(vid:draft, datetime, x, y, km)

              xyint <- rbind(xyint, xyi)
            }
            nrow(xyint)
            xyint$x

            # Determine grid matches ===================================================
            i=1
            grid_id <- c()
            for(i in 1:nrow(xyint)){
              (xyi <- xyint[i,])
              (matchi <- which(grids$x1 <= xyi$x &
                                 grids$x2 > xyi$x &
                                 grids$y1 <= xyi$y &
                                 grids$y2 > xyi$y))
              grid_id[i] <- ifelse(length(matchi)>0, grids$id[matchi[1]], NA)
            }
            grid_id
            xyint$grid_id <- grid_id
            xyint %>% head

            # Filter to points that have a grid match
            xyint <- xyint %>% dplyr::filter(!is.na(grid_id))
            xyint$grid_id

            # Ensure a single grid match per transit row ===============================
            suppressMessages({
              nrow(xyint)
              xyint <-
                xyint %>%
                dplyr::group_by(grid_id) %>%
                dplyr::summarize(across(vid:draft,unique),
                                 datetime = mean(datetime),
                                 x = x[1],
                                 y = y[1],
                                 km = sum(km)) %>%
                arrange(datetime) %>%
                mutate(year = lubridate::year(datetime),
                       month = lubridate::month(datetime),
                       yday = lubridate::yday(datetime))
              nrow(xyint)
            })

            # Add to plot, if you want =================================================
            if(toplot){
              lines(xyint$y ~ xyint$x, col=adjustcolor('grey40', alpha=.3))
              points(x=grids$x[grids$id %in% grid_id],
                     y=grids$y[grids$id %in% grid_id],
                     cex=.5, col=adjustcolor('grey40', alpha=.3))
            }

            # Add to building dataframe
            vgrids <- rbind(vgrids, xyint)
          } # end of if there are at least 2 entries for this day-year-vid
        } # end of yday loop for each vid in each year
      } # end of year loop for each vid
      if(length(vids)>1 & verbose == TRUE){setTxtProgressBar(pb, vi)} # update progress bar
    } # end of vid loop
    message('')
  } # end of vesstype loop

  #library(geosphere) # other option considered
  #(meters <- geosphere::dist2Line(p = c(gridi$x, gridi$y), line = xy)[1])


  return(vgrids)
}
