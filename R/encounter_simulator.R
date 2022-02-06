#' Encounter rate simulator
#'
#' This function performs 1 iteration of the encounter simulator. Each iteration contains `B` simulation runs.
#' To produce a distribution of encounter rate estimates, nest this function call within a `for` loop.
#' See the `vignette` for details.
#'
#' @param params.ship A spatially weighted `data.frame` of vessel characteristics,
#' as produced by `shipstrike::summarize_grid()`. This dataset should have
#' three columns: `v.ship` (speed, in knots), `l.ship` (length, in meters), and `w.ship` (beam width, in meters).
#' @param v.whale A numeric vector of whale travel speeds, in meters per second;
#' this can be a single value or a distribution of values.
#' @param l.whale A numeric vector of whale lengths, in meters;
#' this can be a single value or a distribution of values.
#' @param delta.sd A numeric vector of directivity values (standard deviation of course changes over a 60-second period);
#' this can be a single value or a distribution of values.
#' @param B Number of simulation runs (100 is recommended).
#' @param encounter_meters The proximity between whale and ship to regard as a close encounter; default is 0 meters.
#' @param speedy If `TRUE`, detailed results will not be returned in order to save time.
#' @param toplot If `TRUE`, plots will be printed.
#' @param save_records If `TRUE`, detailed records of every single run will be saved.
#' This is ignored if `speedy` is `TRUE`.
#' @param verbose Boolean; if `TRUE`, progress updates will be printed to the Console.
#' @param plot_timeseries Deprecated.
#'
#' @return A list with the slot `summary` and, if `speedy` is `FALSE` and `save_records` is `TRUE`, the slot `details`.
#' The `summary` slot provides a synopsis of each simulation run. To summarize the results of your simulator run,
#' next go to the function `shipstrike::encounter_tally()`.
#'
#' @export
#'
encounter_simulator <- function(params.ship,
                                v.whale,
                                l.whale,
                                w.whale,
                                delta.sd,
                                B=100,
                                encounter_meters=0,
                                speedy=TRUE,
                                toplot=TRUE,
                                save_records=FALSE,
                                verbose=FALSE,
                                plot_timeseries=FALSE){

  #########################################################
  #########################################################
  # Wrapper function to iterate

  if(FALSE){
    # Parameters
    v.ship = 5 # m/s
    l.ship = 220 # meters
    w.ship = 30 # meters
    params.ship <- data.frame(v.ship,l.ship,w.ship)
    v.whale = 1.5 # m/s
    l.whale = 25 # meters
    w.whale = .2074
    delta.sd = 50 # degr
    toplot=TRUE
    save_records=TRUE
    speedy=TRUE
    verbose=TRUE
    plot_timeseries=FALSE
    encounter_meters=0
    B <- 100
  }

  #########################################################
  #########################################################
  # Convert ship speed from knots to meters per second

  params.ship$v.ship <- params.ship$v.ship * 0.51444

  #########################################################
  #########################################################
  # Internal functions

  # Establish arena

  make.arena <- function(new=TRUE,r=564.18958354776){
    #coords <- make.arena()
    #buffer <- make.arena(new=FALSE,r=750) # Buffer zone
    #library(plotrix)
    #A <- pi*r^2 ; A
    par(mar=c(1,1,1,1))
    if(new){
      suppressWarnings(plot(1~1,col="white",xlim=c(-700,700),ylim=c(-700,700),axes=FALSE,ann=FALSE))
    }
    coords <- plotrix::draw.circle(x=0,y=0,radius=r,nv=360,border="grey")
    coords
    arena <- data.frame(x=coords$x,y=coords$y)
    head(arena)
    return(arena)
  }


  #########################################################
  #########################################################
  # Get ship course

  ship.course <- function(coords,toplot=FALSE){
    #toplot <- TRUE
    ymin_i <- which.min(coords$y) ; ymin_i
    ymax_i <- which.max(coords$y) ; ymax_i
    x1 <- coords$x[ymin_i]
    x2 <- coords$x[ymax_i]
    y1 <- coords$y[ymin_i]
    y2 <- coords$y[ymax_i]
    if(toplot){segments(x0=x1,x1=x2,y0=y1,y1=y2)}
    m <- y2 - y1
    hdg <- 0
    return(list(m=m,hdg=hdg,
                x1=x1,x2=x2,
                y1=y1,y2=y2))
  }

  #########################################################
  #########################################################
  # Get ship timeline

  ship.timeline <- function(course,v){
    #course <- ship.course(coords) ; course
    #v = 5 # m/s
    course
    m <- course$m ; m
    x1 <- course$x1
    x2 <- course$x2
    y1 <- course$y1
    y2 <- course$y2
    secs <- round( m / v ) ; secs

    xint <- seq(x1,x2,length=secs) ; xint
    yint <- seq(y1,y2,length=secs) ; yint

    t <- 1:secs
    xlm <- lm(xint~t)
    ylm <- lm(yint~t)

    tbuff <- -60:(secs+60)
    xbuff <- predict(xlm,newdata=data.frame(t=tbuff))
    ybuff <- predict(ylm,newdata=data.frame(t=tbuff))
    points(x=xbuff,y=ybuff,pch=16,cex=.2,col="grey")

    ins <- rep(1,times=length(tbuff))
    ins[tbuff < 0] <- 0
    ins[tbuff > secs] <- 0
    ins

    tl <- data.frame(t=tbuff,x=xbuff,y=ybuff,status=ins,v=v, hdg=0)
    #tl
    return(tl)
  }

  #########################################################
  #########################################################
  # Draw ellipse function

  ellipse <- function(x,y,
                      width,height=width,theta=0,
                      npoints=10,plot=T) {
    #ellipse(x=0, y=-500, width=.15*200, height=200, theta=310)

    theta <- -1*theta*(pi/180)
    a <- width/2
    b <- height/2
    xcoord <- seq(-a,a,length=npoints)
    ycoord.neg <- sqrt(b^2*(1-(xcoord)^2/a^2))
    ycoord.pos <- -sqrt(b^2*(1-(xcoord)^2/a^2))
    xx <- c(xcoord,xcoord[npoints:1])
    yy <- c(ycoord.neg,ycoord.pos)
    x.theta <- xx*cos(2*pi-theta)+yy*sin(2*pi-theta)+x
    y.theta <- yy*cos(2*pi-theta)-xx*sin(2*pi-theta)+y
    if(plot){polygon(x.theta,y.theta,density=0)}
    return(list(coords=data.frame(x=x.theta,y=y.theta),
                center=c(x,y),
                theta=theta))
  }

  #########################################################
  #########################################################
  # Add ship dimensions

  ship.polys <- function(course,tl,l,w,toplot=FALSE){
    #coords <- make.arena()
    #sc  <- ship.course(coords) ; sc
    #course <- sc
    #v <- 5
    #tl <- ship.timeline(course=sc,v=v)
    #l <- 200
    #w <- 0.15*l
    #course=sc ; tl=tl ; l=lship ; w=wship ; toplot=toplot
    #tship <- ship.polys(course,tl,l,w)
    #tship[[1]]
    #make.arena()
    #tship <- ship.polys(course,tl,l,w,toplot=TRUE)
    #tship[[1]]
    course

    # Establish base ellipse
    x <- tl$x[1] ; x
    y <- tl$y[1] ; y
    ei <- ellipse(x=x, y=y, width=w, height=l, theta=tl$hdg[1], plot=F)
    ei
    ei_global <<- ei

    # Determine y offsets with every step
    tl$dy <- tl$y - tl$y[1] ; tl$dy
    tl$hdg <- course$hdg
    tl$l <- l
    tl$w <- w
    tl

    # Make polys
    polys <- lapply(tl$dy, function(dyi){
      new_ei <- ei_global
      new_ei$coords$y <- new_ei$coords$y + dyi
      new_ei
    })
    polys[[1]]
    polys[[100]]

    head(tl)
    tl$i <- 1:nrow(tl)

    ships <- list(tl=tl,polys=polys)

    # Confirm it worked
    if(FALSE){
      length(ships)
      ships$polys[[1]]
      ships$polys[[2]]
      ships$polys[[100]]

      make.arena()
      lines(ships$polys[[2]]$coords$x,
            ships$polys[[2]]$coords$y)

      lines(ships$polys[[100]]$coords$x,
            ships$polys[[100]]$coords$y)

      lines(ships$polys[[200]]$coords$x,
            ships$polys[[200]]$coords$y)
    }

    return(ships)
  }

  #########################################################
  #########################################################
  #########################################################
  #########################################################
  #########################################################
  #########################################################
  # WHALES

  #########################################################
  #########################################################
  # Get grid of starts

  get.starts <- function(coords){
    #whalestarts <- get.starts(coords)

    x <- seq(-1000,1000,10) ; x
    y <- seq(-1000,1000,10) ; y
    grid <- expand.grid(x,y)
    names(grid) <- c("x","y") ; head(grid)
    nrow(grid)

    #library(sp)
    sgrid = sp::SpatialPoints(grid)

    # Arena
    P1 = sp::Polygon(coords)
    Ps1 = sp::Polygons(list(P1), ID = "a")
    SPs = sp::SpatialPolygons(list(Ps1))

    ins <- sp::over(x=sgrid,y=SPs)
    grid <- grid[which(ins==1),] ; nrow(grid)

    #points(x=grid$x,y=grid$y,pch=16,cex=.2)
    return(grid)
  }



  #########################################################
  #########################################################
  # Initiate whale

  initiate.whale <- function(whalestarts,toplot=FALSE){
    #w0 <- initiate.whale(whalestarts) ; w0

    # Starting point
    i <- sample(1:nrow(whalestarts),1) ; i
    wi <- whalestarts[i,] ; wi
    if(toplot){points(x=wi$x,y=wi$y,col=adjustcolor("firebrick",alpha=.6),pch=16)}

    # Initial bearing
    bi <- sample(1:360,1) ; bi

    return(list(x=wi$x,y=wi$y,hdg=bi))
  }


  #########################################################
  #########################################################
  # Step whale


  step.whale <- function(w0,v,delta.mean=0,delta.sd,toplot=FALSE){

    #v <- 1.3 # m/s = 4.7 kmh
    #delta.mean <- 0 # change in bearing per 1 minute
    #delta.sd <- 5 # change in bearing per 1 minute
    #step.whale(w0=w0,v=1.3,delta.sd=5)

    m <- v*60 ; m
    dhdg <- rnorm(1,mean=delta.mean,sd=(delta.sd*(pi/180))) ; dhdg

    b0 <- w0$hdg  ; b0
    b0 <- b0 * (pi/180)
    b1 <- b0 + dhdg

    dy <- sin(b1+pi/2)*m ; dy
    dx <- cos(2*pi - b1 + pi/2)*m ; dx

    x <- w0$x + dx
    y <- w0$y + dy

    if(!is.finite(x)){x <- 1}

    xint <- seq(w0$x,x,length=60) ; xint
    yint <- seq(w0$y,y,length=60) ; yint

    if(toplot){points(x=xint,y=yint,pch=1,col="red",cex=.2)}
    if(toplot){points(x=x,y=y,pch=16,col="red",cex=1)}
    w1 <- list(x=xint,
               y=yint,
               hdg=(b1*(180/pi)) %% 360,
               v=v)
    return(w1)
  }


  #########################################################
  #########################################################
  # Whale track

  whale.trax <- function(w0,t,v,delta.mean=0,delta.sd,toplot=FALSE){

    #t <- 3000
    #v <- 1.5 # m/s
    #delta.mean <- 0 # change in bearing per 1 minute
    #delta.sd <- 70 # change in bearing per 1 minute
    #w0
    #trax <- whale.trax(w0=w0,t=nrow(tl),v=1.3,delta.sd=5)

    wi <- w0
    if(toplot){points(wi$x,wi$y,col=adjustcolor('firebrick',alpha=.6),cex=1)}
    steps <- round(t/60)
    trax <- data.frame()
    i=1
    for(i in 1:steps){
      stepi <- step.whale(w0=wi,v=v,delta.sd=delta.sd) ; stepi
      si <- data.frame(step=i,x=stepi$x,y=stepi$y,hdg=stepi$hdg, v=v) ; si
      trax <- rbind(trax,si)
      wi <- list(x=tail(stepi$x,1),
                 y=tail(stepi$y,1),
                 hdg=stepi$hdg,
                 v=v); wi
    }

    nrow(trax)
    trax$t <- 1:nrow(trax)
    return(trax)
  }

  #########################################################
  #########################################################
  # Create whale polygons

  whale.polys <- function(trax,l,w,toplot=FALSE){
    #trax <- whale.trax(w0=w0,t=nrow(tl),v=1.3,delta.sd=5)
    #trax
    #l <- 20
    #w <- 5  #twhale <- whale.polys(trax,l,w,toplot=TRUE)

    whales <- list()
    i=25
    for(i in 1:nrow(trax)){
      traxi <- trax[i,] ; traxi
      x <- traxi$x
      y <- traxi$y
      hdg <- traxi$hdg ; hdg
      ei <- ellipse(x=x, y=y, width=w, height=l, theta=hdg, plot=F)
      whales[[i]] <- list(t=traxi$t,
                          v=traxi$v,
                          hdg=hdg,
                          center=ei$center,
                          xy=ei$coords)
    }
    return(whales)
  }


  #########################################################
  #########################################################

  find_bow <- function(ei,hypo=300, toplot=FALSE){
    #ei <- tship[[1]]
    #hypo <- 300

    # Build heading arrow
    ctr <- ei$center
    x <- ctr[1]
    y <- ctr[2]
    theta <- (ei$theta * (pi/180))

    dx <- cos(2*pi - theta + pi/2)*hypo ; dx
    dy <- sin(theta + pi/2)*hypo ; dy
    hdg_x <- seq(x,(x+dx),length=1000)
    hdg_y <- seq(y,(y+dy),length=1000)
    if(toplot){lines(hdg_x,hdg_y,col='grey')}

    # Find bow
    pythag <- function(c1,c2){
      c2 <- data.frame(x=hdg_x, y=hdg_y)
      dx <- abs(c1[1] - c2[1])
      dy <- abs(c1[2] - c2[2])
      sqrt(dx^2 + dy^2)
    }
    ds <- apply(ei$coords,1,pythag)
    bow_i <- which.min(unlist(lapply(ds,min))) ; bow_i
    bow <- ei$xy[bow_i,]
    if(toplot){points(x=bow$x, y=bow$y, col='firebrick', pch=16)}

    ei_return <- list(center=ctr,
                      hdg=ei$theta * (180/pi),
                      hdg_line=data.frame(x=hdg_x, y=hdg_y),
                      theta=theta,
                      bow=bow)
    return(ei_return)
  }

  #########################################################
  #########################################################

  find_whale <- function(whale,hypo=100, toplot=FALSE){

    #whale <- whale_min
    #hypo=100

    # Build heading arrow
    x <- whale$x
    y <- whale$y
    if(toplot){points(x,y,col='blue',pch=16)}
    theta <- (whale$hdg * (pi/180))

    dx <- cos(2*pi - theta + pi/2)*hypo ; dx
    dy <- sin(theta + pi/2)*hypo ; dy

    bow_hdg_x <- seq(x,(x+dx),length=1000)
    bow_hdg_y <- seq(y,(y+dy),length=1000)
    if(toplot){lines(bow_hdg_x,bow_hdg_y,col='red')}

    tail_hdg_x <- seq(x,(x-dx),length=1000)
    tail_hdg_y <- seq(y,(y-dy),length=1000)
    if(toplot){lines(tail_hdg_x,tail_hdg_y,col='green')}

    ei_return <- list(whale=whale,
                      bow_hdg_line=data.frame(x=bow_hdg_x, y=bow_hdg_y),
                      tail_hdg_line=data.frame(x=tail_hdg_x, y=tail_hdg_y))
    return(ei_return)
  }

  #########################################################
  #########################################################
  # Determine overlap of polygons

  encounter.test <- function(tship,twhale,speedy=TRUE){
    #library(sf)

    if(FALSE){
      coords <- make.arena()
      course <- ship.course(coords) ; course
      v = 5 # m/s
      sc  <- ship.course(coords) ; sc
      tl <- ship.timeline(course=sc,v=v)
      l <- 200
      w <- 0.15*l
      tship <- ship.polys(sc,tl,l,w,toplot=TRUE)
      whalestarts <- get.starts(coords)
      w0 <- initiate.whale(whalestarts) ; w0
      v <- 1.3 # m/s = 4.7 kmh
      delta.mean <- 0 # change in bearing per 1 minute
      delta.sd <- 30 # change in bearing per 1 minute
      trax <- whale.trax(w0=w0,t=nrow(tl),v=v,delta.sd=delta.sd)
      twhale <- whale.polys(trax,l,w)
    }

    # Ship timestamps
    ts <- tship$tl
    head(ts)

    # Whale timestamps
    tw <- twhale
    tw$i <- 1:nrow(tw)
    tw

    # Base test
    near_test <- which(tw$x > (-1*ts$w[1] - 20) &
                         tw$x < (ts$w[1] + 20))
    near_test

    prox <- NULL
    if(speedy & length(near_test) > 0){

      # The whale occurs within 20m of the ship's width ==========================

      # Subset whale to moments at that proximity
      tw <- tw[near_test,] ; nrow(tw)

      # Which timestamps do the two things share?
      commont <- tw$t[which(tw$t %in% ts$t)] ; commont

      if(length(commont) > 0){ # is the whale in the center area when the ship is actually in the arena?

        ts_common <- ts[ts$t %in% commont,] ; nrow(ts_common)
        tw_common <- tw[tw$t %in% commont,] ; nrow(tw_common)

        # Subset tracks to those common timestamps
        keeps <- ts_common$i ; keeps
        spoly_raw <- tship$polys[keeps]

        # Extract the ellipse shapes for those timestamps
        spoly <- lapply(spoly_raw,function(tsii){
          si <- tsii$coords
          c1 = cbind(si$x, si$y)
          r1 = rbind(c1, c1[1, ])
          sip <- sf::st_polygon(list(r1)) ; sip
          return(sip)
        })
        #spoly[[1]]
        #head(twi)

        tw_global <<- tw_common
        proximities <- sapply(1:nrow(tw_common),function(i){
          twii <- tw_global[i,2:3] ; twii
          spoli <- spoly[[i]] ; spoli
          twii_pt <- sf::st_point(as.numeric(twii))
          sf::st_distance(spoli,twii_pt)[1,1]
        })
        proximities
        #par(mar=c(4.2,4.2,.5,.5)) ; plot(proximities,ylim=c(0,1500),type='l')

        # Create encounter summary
        enc <- data.frame(t = tw_common$t,
                          meters = proximities,
                          whale_hdg = tw_common$hdg,
                          whale_v = tw_common$v,
                          whale_x = tw_common$x,
                          whale_y = tw_common$y,
                          ship_hdg = ts_common$hdg,
                          ship_v = ts_common$v,
                          ship_x = ts_common$x,
                          ship_y = ts_common$y)
        enc

        # Get moment of closest proximity
        min_i <- which.min(proximities) ; min_i
        proximities[min_i]
        ship_min <- spoly_raw[min_i][[1]] ; ship_min

        # Add bow / stern info
        enc$ship_y_bow <- max(ship_min$coords$y)
        enc$ship_y_stern <- min(ship_min$coords$y)

        whale_min <- tw_common[min_i,]
        whale_min

        if(FALSE){
          make.arena()
          points(x=tw_common$x, y=tw_common$y,type='l')
          points(x=ts_common$x, y=ts_common$y,type='l')
          lines(x=ship_min$coords$x,
                y=ship_min$coords$y)
        }

        enc[min_i,]

        # Compile proximity object ===================================================
        prox <- list()
        prox$timeline <- enc
        prox$ship <- ship_min
        prox$whale <- whale_min
      }
    }
    return(prox)
  }

  #########################################################
  #########################################################

  # Setup
  coords <- make.arena(new=TRUE)
  whalestarts <- get.starts(coords)

  # Establish ship course
  coords <- make.arena(new=FALSE)
  sc <- ship.course(coords)

  # Process
  b <- 1
  MR <- data.frame()
  records <- list()
  hits <- list()
  for(b in 1:B){

    # Setup params =============================================================

    if(verbose){n <- Sys.time() ; message(b,' --- ',n,' :: pick params ...')}
    ship_row <- 1
    if(nrow(params.ship)>1){ship_row <- sample(1:nrow(params.ship),1)}
    vs <- params.ship$v.ship[ship_row]
    lship <- params.ship$l.ship[ship_row]
    wship <- params.ship$w.ship[ship_row]
    v <- v.whale ; if(length(v.whale)>1){v <- sample(v.whale,1)}
    deltasd <- delta.sd ; if(length(delta.sd)>1){deltasd <- sample(delta.sd,1)}
    lwhale <- l.whale ; if(length(l.whale)>1){lwhale <- sample(l.whale,1)}
    wwhale <- lwhale * w.whale

    # Ship course & timing =====================================================

    if(verbose){newn <- Sys.time() ; diffn <- difftime(newn,n,units='secs')
    n <- newn ; message(b,' --- ',diffn,' :: simulate ship ...')}
    tl <- ship.timeline(course=sc,v=vs)
    tship <- ship.polys(course=sc,
                        tl=tl,
                        l=lship,
                        w=wship,
                        toplot=toplot)
    #names(tship)
    #tship$polys[[1]]
    #tship$polys[[100]]
    #tship$polys[[length(tship$polys)]]
    #length(tship)
    #length(tship$polys)

    # Whale course & timing ====================================================

    if(verbose){newn <- Sys.time() ; diffn <- difftime(newn,n,units='secs')
    n <- newn ; message(b,' --- ',diffn,' :: simulate whale ...')}
    w0 <- initiate.whale(whalestarts,toplot=toplot)
    trax <- whale.trax(w0=w0,
                       t=nrow(tl),
                       v=v,
                       delta.sd=deltasd)
    if(toplot){lines(x=trax$x,y=trax$y,col=adjustcolor('firebrick',alpha=.4))}
    twhale <- trax

    # Stage results dataframe ==================================================

    encounter <- list()
    encounter$summary <- data.frame(run=b,
                                    encounter=0,
                                    closest=NA,
                                    whale_hdg=NA,
                                    whale_x=NA,
                                    whale_y=NA,
                                    whale_v=v,
                                    whale_l=lwhale,
                                    whale_w=wwhale,
                                    whale_deltasd=deltasd,
                                    ship_v=vs,
                                    ship_l=lship,
                                    ship_w=wship,
                                    ship_x=NA,
                                    ship_y=NA,
                                    ship_y_bow=NA,
                                    ship_y_stern=NA)
    encounter$timeline <- data.frame()

    # Was there an imminent encounter? =========================================

    if(verbose){newn <- Sys.time() ; diffn <- difftime(newn,n,units='secs')
    n <- newn ; message(b,' --- ',diffn,' :: test for encounter ...')}
    encounter_verdict <- encounter.test(tship,twhale,speedy=speedy)
    encounter_verdict

    if(!is.null(encounter_verdict)){
      min_i <- which.min(encounter_verdict$timeline$meters)
      encounter$summary$closest <- encounter_verdict$timeline$meters[min_i]
      encounter$summary$encounter <- ifelse(encounter$summary$closest==encounter_meters,1,0)
      encounter$summary$whale_hdg <- encounter_verdict$timeline$whale_hdg[min_i]
      encounter$summary$whale_x <- encounter_verdict$timeline$whale_x[min_i]
      encounter$summary$whale_y <- encounter_verdict$timeline$whale_y[min_i]
      encounter$summary$ship_x <- encounter_verdict$timeline$ship_x[min_i]
      encounter$summary$ship_y <- encounter_verdict$timeline$ship_y[min_i]
      encounter$summary$ship_y_bow <- encounter_verdict$timeline$ship_y_bow[min_i]
      encounter$summary$ship_y_stern <- encounter_verdict$timeline$ship_y_stern[min_i]

      if(plot_timeseries){plot(encounter_verdict$timeline$meters,
                               ylim=c(0,1200),type='l',col='firebrick')}

      if(save_records){
        records[[b]] <- encounter_verdict
      }
    }

    # Store summary ============================================================

    if(verbose){newn <- Sys.time() ; diffn <- difftime(newn,n,units='secs')
    n <- newn ; message(b,' --- ',diffn,' :: add to results list ...')}
    MR <- rbind(MR,encounter$summary)

    # Store record =============================================================

    if(verbose){message('--- sub-run ',b)}
  }

  result_list <- list(summary=MR,
                      records=records)
  return(result_list)
}
