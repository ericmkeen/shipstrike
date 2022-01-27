#' Interpolate vessel fixes and associate with grid cells
#'
#' @param grids  desc
#' @param vessels  desc
#' @param toplot  desc
#' @param verbose  desc
#'
#' @return
#' @export
#'
vessel_grid <- function(grids,
                        vessels,
                        toplot = FALSE,
                        verbose=TRUE){
  #toplot = TRUE
  head(grids)
  head(vessels)

  # Stage result
  vgrids <- data.frame()

  # Loop through each unique vessel type
  (vtypes <- vessels$type %>%  unique())
  vtypi <- 2
  for(vtypi in 1:length(vtypes)){
    (vtype <- vtypes[vtypi])
    vesstype <- vessels %>% dplyr::filter(type == vtype)
    nrow(vesstype)
    if(verbose){message('Vessel type ',vtype,' :: Interpolating fixes & determining grid cell associations ...')}

    if(toplot){
      plot(x=grids$x, y=grids$y, cex=.1, pch=16)
    }

    # Loop through each unique vessel in this type
    vids <- vesstype$vid %>% unique()
    pb <- txtProgressBar(1, length(vids), style=3) # setup progress bar
    vi=1
    for(vi in 1:length(vids)){
      (vidi <- vids[vi])
      (xy <- vesstype %>% dplyr::filter(vid==vidi))

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

      # Ensure a single grid match per transit row ===============================
      nrow(xyint)
      xyint <-
        xyint %>%
        dplyr::group_by(grid_id) %>%
        dplyr::summarize(across(vid:draft,unique),
                         datetime = mean(datetime),
                         x = x[1],
                         y = y[1]) %>%
        arrange(datetime)
      nrow(xyint)

      # Add to plot, if you want =================================================
      if(toplot){
        lines(xyint$y ~ xyint$x, col=adjustcolor('grey40', alpha=.2))
        points(x=grids$x[grids$id %in% grid_id],
               y=grids$y[grids$id %in% grid_id],
               cex=.5, col=adjustcolor('grey40', alpha=.2))
      }

      # Add to building dataframe
      vgrids <- rbind(vgrids, xyint)
      setTxtProgressBar(pb, vi) # update progress bar
    } # end of vid loop
    message('')
  } # end of vesstype loop

  #library(geosphere) # other option considered
  #(meters <- geosphere::dist2Line(p = c(gridi$x, gridi$y), line = xy)[1])

  return(vgrids)
}
