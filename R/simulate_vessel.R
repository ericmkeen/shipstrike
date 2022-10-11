#' Simulate a set of vessel transits on a spatial grid
#'
#' This function is deprecated but may still be useful in some contexts.
#'
#' @param grids A spatial grid of cells, as produced by `shipstrike::make_grid()` (see documentation)
#' (or at least matching the format thereof).
#' @param vessels A list of lists, each one representing settings for the simulation of a set of vessel transits.
#' The expected format of each list is as follows:
#' \itemize{
#' \item `type` A chatacter string name for this type of vessel, whatever you want: e.g., `"fishing"`, `"tanker"`, etc.
#' \item `n` A numeric indicating the number of transits to simulate.
#' \item `size_min` Minimum vessel size (meters)
#' \item `size_max` Maximum vessel size (meters); for each transit, a size will be
#' drawn from a uniform-random distribution between the min and the max.
#' \item `speed_min` Minimum vessel speed (knots)
#' \item `speed_max` Maximum vessel speed (knots); as with size, the speed of each transit will
#' be drawn from a uniform random distribution between the min and the max.
#' \item `width` A scaling factor, between 0 and 1, used to scale the length of the vessel
#' to get its beam width. Typical values for a large ship range from 0.05 to 0.15.
#' \item `draft` A scaling factor, between 0 and 1, used to scale the length of the vessel
#' to get its draft. Typical values are somewhere between 0.03 and 0.10.
#' \item `hour` The hour of day on which to begin the transit; if not provided or `NULL`,
#' a random start hour will be chosen.
#' \item `months` The months of year in which to simualte transits; if nothing is provided, or if `NULL`,
#' the month for each transit will be randomly chosen.
#' This can be handy if you want to simulate the effects of adding vessel traffic at only certain times of year.
#' Regardless of whether `month` is specified, the day of month for each transit is then randomly chosen.
#' \item `direction` A means of alternating or reversing the direction transited along the vessel's course; ignored if
#' `course` below is `NULL`. If `direction` is `0` or `NULL` or missing,
#' the vessel always travels along the course in the sequence indicated
#' within the `course` `data.frame`, starting at the first pair of coordinates.
#' If `direction` is `1`, the course will be reversed.
#' If `direction` is `2`, the course direction will alternate randomly.
#' \item `course_var` A means of adding minor variation to the `course` `data.frame` provided below;
#' will be ignored if `course` is missing or `NULL`. The argument `course_var` accepts a small
#' numeric (typically less than 0.005, for reasonable results), representing degrees latitude;
#' that number will be used to offset the `course` coordinates by `rnorm(1, mean=0, sd=course_var)`
#' in both the north-south and east-west directions.
#' \item `course` A `data.frame` of coordinates, representing the planned route for this
#' group of vessels. If provided, it must have two columns: `x` (longitude, decimal degrees
#' in which West is negative) and `y` (latitude, decimal degrees in which South is negative).
#' If not provided, the route will be randomly created as a linear line crossing the spatial grid.
#' }
#' @param toplot A Boolean; if `TRUE`, progress plots will be shown.
#' @param verbose A Boolean; if `TRUE`, updates will be printed to the Console.
#'
#' @return A `data.frame`, in which each row is a position fix for a transiting vessel,
#' with the following columns:
#' \itemize{
#' \item `vid` = A fake ID for the vessel
#' \item `type` = Vessel type
#' \item `speed` = Speed, in knots
#' \item `length` = Length
#' \item `width` = Beam width
#' \item `draft` = Draft
#' \item `datetime` = Datetime in UTC, with format `yyyy-mm-dd hh:mm:ss`.
#' \item `x` = Longitude, decimal degrees (Western degrees negative).
#' \item `y` = Latitude, decimal degrees (Southern degrees negative).
#' \item `km` = The distance, in kilometers, between this and the next position fix.
#' }
#' @export
#'
simulate_vessel<- function(grids,
                           vessels,
                           toplot=TRUE,
                           verbose=TRUE){
  # debugging
  if(FALSE){

    cfv <- list(type='fishing',
                n=40,
                size_min = 20,
                size_max = 50,
                speed_min = 6,
                speed_max = 15,
                width = 0.15,
                draft = 0.1,
                hour = 4,
                months = 1:12,
                direction = 2,
                course_var = NULL,
                course = NULL)

    # Prepare tanker route
    data(shiplane) # bangarang package
    tanker1 <- shiplane %>% dplyr::filter(PID==1, POS < 51)
    tanker2 <- shiplane %>% dplyr::filter(PID==3, POS < 51)
    (tanker_route <- rbind(tanker1, tanker2) %>% select(x=X, y=Y))

    tanker <- list(type='tanker',
                   n=100,
                   size_min = 200,
                   size_max = 350,
                   speed_min = 10,
                   speed_max = 12,
                   width = 0.15,
                   draft = 0.1,
                   hour = NULL,
                   months = 1:12,
                   direction = 0, # 0 = reg; 1 = rev; 2 = random
                   course_var = .002,
                   course = tanker_route)

    vessels <- list(tanker)
    verbose <- TRUE
  }

  returns <- data.frame()
  vi = 1
  fake_id <- 1
  for(vi in 1:length(vessels)){
    (vessi <- vessels[[vi]])

    vtype <- vessi$type
    if(verbose){message('Simulating ',vtype,' vessels ....')}
    (vn <- vessi$n)

    if(verbose){pb <- txtProgressBar(1, vn, style=3)} # setup progress bar
    i=1
    for(i in 1:vn){

      vlength <- runif(1, vessi$size_min, vessi$size_max)
      vwidth <- vlength * vessi$width
      vdraft <- vlength * vessi$draft
      vspeed <- runif(1, vessi$speed_min, vessi$speed_max)
      (hh <- ifelse(is.null(vessi$hour),sample(0:23,1),vessi$hour))
      if(is.null(vessi$months)){months <- 1:12}else{months <- vessi$months} ; months
      (vdir <- ifelse(is.null(vessi$direction),0,vessi$direction))
      if(is.null(vessi$course_var)){course_var <- 0}else{course_var <- vessi$course_var} ; course_var

      # If course is not provided, make one up
      xy <- data.frame()
      if(!is.null(vessi$course)){
        xy <- vessi$course
        (xerror <- rnorm(1,mean=0,sd=course_var))
        (yerror <- rnorm(1,mean=0,sd=course_var))
        xy$x <- xy$x + xerror
        xy$y <- xy$y + yerror
      }else{
        if(runif(1,0,1) < 0.5){
          # Randomize Ys
          x1 <- min(grids$x) ; x2 <- max(grids$x)
          (y1 <- sample(grids$y, size=1))
          (y2 <- sample(grids$y, size=1))
        }else{
          # Randomize Xs
          (x1 <- sample(grids$x, size=1))
          (x2 <- sample(grids$x, size=1))
          y1 <- min(grids$y) ; y2 <- max(grids$y)
        }
        xy <- stats::approx(x=c(x1,x2), y=c(y1,y2), n=10) %>% data.frame
      }
      xy

      # Handle direction
      if(vdir == 1){ xy <- xy[nrow(xy):1, ,] } # reverse
      if(vdir == 2){ # randomly decide whether to reverse it
        if(runif(1,0,1) < 0.5){ xy <- xy[nrow(xy):1,] }
      }

      # Calculate distance covered
      pos <- data.frame(lat1 = xy$y[1:length(xy$y)-1],
                        lon1 = xy$x[1:length(xy$x)-1],
                        lat2 = xy$y[2:length(xy$y)],
                        lon2 = xy$x[2:length(xy$x)])
      d <- apply(pos,1,function(x){swfscDAS::distance_greatcircle(x[1], x[2], x[3], x[4])})
      xy$km <- c(d,mean(d))
      head(xy)

      # Calculate time span between entries
      xy$hours <- xy$km / vspeed
      xy$hours_cum <- cumsum(xy$hours)
      head(xy)

      # Randomly select start date/time
      (yyyy <- lubridate::year(Sys.time()))
      (mo <- sample(months,size=1))
      (dd <- sample(1:28,size=1))
      # hh is optionally provided by user above
      (mm <- sample(0:59, size=1))
      (dt <- paste0(yyyy,'-',mo,'-',dd,' ',hh,':',mm,'::00'))
      (dt <- lubridate::as_datetime(dt))
      xy$datetime <- dt + lubridate::seconds(xy$hours_cum * 3600)
      head(xy)

      # Plot
      if(toplot){
        if(vi==1 & i==1){ plot(x=grids$x, y=grids$y, cex=.1, pch=16, col='grey') }
        lines(xy$y~xy$x,col=adjustcolor('black',alpha=.1))
      }


      # Prepare result
      returni <-
        xy %>%
        dplyr::mutate(vid = fake_id,
                      type = vtype,
                      speed = vspeed,
                      length = vlength,
                      width = vwidth,
                      draft = vdraft) %>%
        dplyr::select(vid:draft, datetime, x,y, km)

      returns <- rbind(returns, returni)
      fake_id <- fake_id + 1
      if(verbose){setTxtProgressBar(pb, i)} # update progress bar
    }
    message('')
  }
  return(returns)
}
