#' Simulate a set of vessels
#'
#' @param grids desc
#' @param vessels a list of lists
#' @param toplot desc
#' @param verbose desc
#'
#' @return
#' @export
#'
#' @examples
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
                   course = tanker_route)

    vessels <- list(cfv, tanker)
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

    pb <- txtProgressBar(1, vn, style=3) # setup progress bar
    i=1
    for(i in 1:vn){

      vlength <- runif(1, vessi$size_min, vessi$size_max)
      vwidth <- vlength * vessi$width
      vdraft <- vlength * vessi$draft
      vspeed <- runif(1, vessi$speed_min, vessi$speed_max)
      (hh <- ifelse(is.null(vessi$hour),sample(0:23,1),vessi$hour))
      (months <- ifelse(is.null(vessi$months),1:12,vessi$months))
      (vdir <- ifelse(is.null(vessi$direction),0,vessi$direction))

      # If course is not provided, make one up
      xy <- data.frame()
      if(!is.null(vessi$course)){
        xy <- vessi$course
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
      (mo <- sample(1:months,size=1))
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
        lines(xy$y~xy$x)
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
      setTxtProgressBar(pb, i) # update progress bar
    }
    message('')
  }
  return(returns)
}
