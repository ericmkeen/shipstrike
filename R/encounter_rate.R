#' Simulator to estimate whale-vessel close-encounter rate
#'
#' In the `shipstrike` framework, the "close-encounter rate" is
#' the rate at which a vessel and whale are expected to intersect in time and
#' horizontal space assuming no avoidance measures are taken. In other words, it is
#' the share of cooccurrences (i.e., whale and ship occur in same square km) that will
#' lead to immediate overlap if not avoided.
#'
#' The close-encounter rate is influenced by vessel parameters (dimensions, speed, directionality)
#' as well as whale parameters (dimensions, speed, directionality), all of which change throughout the year
#' and by diel period (i.e., daytime or nighttime).
#'
#' This function utilizes simulations to predict the close-encounter rate for each
#' combination of vessel type, diel period, and "month batch" provided.
#' This simulator is described in detail in Keen et al. (2022). It is designed to
#' produce a posterior distribution of the encounter rate estimate.
#'
#' @param vessels A `data.frame` of vessel position fixes. Each row ought to be
#' a position fix within a spatial grid cell for a single transit from a single vessel,
#' with no more than one row per cell-transit-vessel.  Require fields:
#' \itemize{
#' \item `grid_id` = Grid cell identifier.
#' \item `vid` = Unique vessel identifier (numeric)
#' \item `type` = Vessel type (character string)
#' \item `speed` = Vessel speed, in knots (numeric)
#' \item `length` = Vessel length, in meters (numeric)
#' \item `width` = Vessel beam width, in meters (numeric)
#' \item `draft` = Vessel beam width, in meters (numeric)
#' \item `datetime` = Datetime in UTC, with format `yyyy-mm-dd hh:mm:ss`
#' \item `x` = Longitude, decimal degrees (Western degrees negative)
#' \item `y` = Latitude, decimal degrees (Southern degrees negative)
#' \item `year` = Year (yyyy)
#' \item `diel` = Diel period -- either `"day"` or `"night"` (character)
#' }
#'
#' @param whales A `list` providing parameters for whale dimensions, speed, and directivity.
#' For examples, see the `shipstrike` functions `fin_params()` or `humpback_params()`.
#'
#' @param outcome_dir Path specifiying the directory into which the simulator result will be saved.
#' Default is your working directory.
#'
#' @param month_batches A list specifying how months of vessel traffic should be grouped together
#' into seasons.
#'
#'
#' @param speed_restriction Option to apply a maximum speed, in knots, converting all vessel positions
#' that exceed that maximum to the maximum. This is a convenience input that allows you to see the effect of
#' a speed-restriction mitigation measure on the encounter rate.
#'
#' @param lengths_restricted Option to specify the minimum length to which the above speed restriction would apply.
#' This lets you apply the speed restriction only to certain ship lengths.
#'
#' @param new_speeds Another option to applying virtual mitigation measures: specify a new vector
#' of ship speeds (can be length `1` or the same as the number of rows in`vessels`).
#'
#' @param runs The number of runs. Each run produces a single estimate of the encounter rate
#' based on the fraction of `iterations` that results in a close-encounter.
#' Note that if `runs` is `1`, the function will return detailed diagnostics on the simulation.
#' If `runs` is more than `1`, simple results will be returned instead.
#'
#' @param iterations The number of simulations to produce for each run of the simulator.
#' The fraction of these iterations that result in a close-encounter is taked as the estimate
#' of the encounter rate for that run.
#'
#' @return If `runs` is more than `1` (we recommend at least 100 for a
#' minimum acceptable posterior distribution size), a `data.frame` will be returned
#' with 5 fields: `type` (the vessel type), `month`, `diel`, `i` (the run identifier),
#' and `p_encounter` (the estimate of the encounter rate for that run). Each row
#' is the result of a single run.

#' If `runs` is just `1`, a detailed `data.frame` will be returned which
#' can be passed to `shipstrike::encounter_diagnostics` for
#' a data-rich QA/QC overview of the encounter rate simulation process.
#'
#' See the package vignette for examples.
#'
#' @import dplyr
#' @import ggplot2
#' @import sf
#' @export
#'
encounter_rate <- function(vessels,
                           whales,
                           outcome_dir = '',
                           month_batches = list(winter = c(0:4, 11:12),
                                                summer = 5:10),
                           speed_restriction = NULL,
                           lengths_restricted = 0,
                           new_speeds = NULL,
                           runs = 100,
                           iterations = 100,
                           toplot = TRUE){

  # debugging ##################################################################

  if(FALSE){
    data(lng_canada)
    vessels <- lng_canada
    (whales <- fin_params())

    outcome_dir <- 'fw/impacts_2019/'

    month_batches = list(winter = c(0:4, 11:12),
                         summer = 5:10)

    (new_speeds <- NULL)
    #(new_speeds <- runif(100, 7, 9))
    speed_restriction = NULL
    #speed_restriction = 10
    lengths_restricted = 0

    runs <- 100 # number of times to run encounter
    iterations <- 100 # number of iterations in each run
    toplot = TRUE
  }

  ##############################################################################
  # Stage inputs

  b <- iterations
  B <- runs

  # Rename sourced data
  vessi <- vessels

  # Modify speeds
  if(!is.null(new_speeds)){
    vessi$speed <- sample(new_speeds, size=nrow(vessi), replace=TRUE)
  }

  if(!is.null(speed_restriction)){
    bads <- which(vessi$length > lengths_restricted & vessi$speed > speed_restriction)
    bads
    #hist(vessi$speed)
    if(length(bads)>0){
      vessi$speed[bads] <- speed_restriction
    }
    #hist(vessi$speed)
  }

  # Stage columns
  if(! 'type' %in% names(vessi)){vessi$type <- 'All'}
  if(! 'diel' %in% names(vessi)){vessi$diel <- 'All'}
  if(! 'month' %in% names(vessi)){vessi$month <- 0}

  if(sum(unique(vessi$month))==0){
    month_batches <- list(all=0:12)
  }
  month_batches
  monthi <- 'summer'
  #monthi <- 'all'

  ################################################################################

  # Stage results for this scenario
  results <- data.frame()

  # Stage results for details (only if runs == 1)
  MR <- data.frame()

  # Loop through each vessel type
  (types <- vessi$type %>% unique %>% sort)
  (typi <- types[1]) #'Tug < 50m'
  for(typi in types){

    # Loop through each month
    monthii <- 1
    for(monthii in 1:length(month_batches)){
      (monthi <- names(month_batches[monthii]))
      (monthi_no <- month_batches[[monthii]])

      vm <- vessi %>% dplyr::filter(type == typi,
                                    month %in% monthi_no)
      nrow(vm)
      if(nrow(vm)>0){

        # Loop through each diel period
        diels <- c('day','night')
        dieli <- 'night'
        for(dieli in diels){
          vmd <- vm %>% dplyr::filter(diel %in% c('All', dieli))
          nrow(vmd)
          if(nrow(vmd)>0){
            message('\nType ',typi,' :: Months of ',monthi,' :: diel period ',dieli,' :: simulating encounters ....')

            # Setup ship parameters ================================================
            params.ship <- apply(vmd, 1, function(vmdi){
              #(vmdi <- vmd[1,])
              vmdi <- vmdi %>% as.character
              (speeds <- stringr::str_split(vmdi[4],'_') %>% unlist %>% as.numeric)
              (lengths <- stringr::str_split(vmdi[5],'_') %>% unlist %>% as.numeric)
              (widths <- stringr::str_split(vmdi[6],'_') %>% unlist %>% as.numeric)
              dfi <- data.frame(v.ship = speeds, l.ship = lengths, w.ship = widths)
              return(dfi)
            })
            params.ship <- bind_rows(params.ship)
            head(params.ship)
            nrow(params.ship)

            # Draw random values for whale parameters
            (l.whale <- truncnorm::rtruncnorm(10000,
                                              a = whales$length_min,
                                              b = whales$length_max,
                                              mean = whales$length_mean,
                                              sd = whales$length_sd))
            (w.whale <- whales$width_factor) #l.whale * width_factor)
            if(dieli == 'day'){
              (v.whale <- truncnorm::rtruncnorm(10000,
                                                a = whales$speed_day_min,
                                                b = whales$speed_day_max,
                                                mean = whales$speed_day_mean,
                                                sd = whales$speed_day_sd))
              (delta.sd <- rnorm(10000,
                                 mean = whales$delta_day_mean,
                                 sd = whales$delta_day_sd))
            }else{
              (v.whale <- truncnorm::rtruncnorm(10000,
                                                a = whales$speed_night_min,
                                                b = whales$speed_night_max,
                                                mean = whales$speed_night_mean,
                                                sd = whales$speed_night_sd))
              (delta.sd <- rnorm(10000,
                                 mean = whales$delta_night_mean,
                                 sd = whales$delta_night_sd))
            }
            (delta.sd <- exp(delta.sd)) # reverse log-transform

            ####################################################################
            ####################################################################

            # Begin encounter simulation loops =====================================

            if(runs > 1){ pb <- txtProgressBar(1, B, style=3) } # setup progress bar
            par(mfrow=c(2,2))
            p_encounters <- c()
            Bi <- 1
            for(Bi in 1:B){
              if(runs > 1){ setTxtProgressBar(pb, Bi) } # update progress bar

              # Create arena  ====================================================
              R <- 564.3185
              arena <-
                data.frame(x=0, y=0) %>%
                sf::st_as_sf(coords=c('x','y')) %>%
                sf::st_buffer(dist=R)
              #sf::st_area(arena) # confirm area

              # Ship tracks ======================================================

              # Create a list of ship courses for each option
              suppressMessages({
                ships <-
                  params.ship %>%
                  # convert ship speed to m/s
                  mutate(v.ship = v.ship * 0.51444) %>%
                  # sample b rows
                  slice_sample(n=b, replace=TRUE) %>%
                  # add identifier
                  mutate(id_ship = 1:n()) %>%
                  # total distance traveled (arena + length of vessel)
                  mutate(mtot = 2*R + l.ship) %>%
                  # total transit duration (bow entering arena to stern leaving it)
                  mutate(stot = ceiling( mtot / v.ship)) %>%
                  group_by(id_ship, v.ship, l.ship, w.ship) %>%
                  # second-by-second bow locations
                  summarize(t = 1:stot,
                            hdg_ship = 0,
                            x_ship = 0,
                            x_port = 0 - w.ship,
                            x_star = 0 + w.ship,
                            y_bow = seq((-1*R), (R + l.ship),
                                        length = stot)) %>%
                  mutate(y_stern = y_bow - l.ship,
                         y_center = y_bow - 0.5*l.ship) %>%
                  group_split()
              })

              # Review
              #ships %>% length
              #ships[[1]]

              # Get longest-running ship course (to use to create whale tracks)
              (tmax <- lapply(ships, function(x){max(x$t)}) %>% unlist %>% max)

              # Whale starting points ============================================

              (whale_starts <- st_sample(arena, size=b) %>% st_coordinates %>% as.data.frame)

              params.whale <-
                data.frame(id_whale = 1:b,
                           v.whale = sample(v.whale, size=b, replace=TRUE),
                           l.whale = sample(l.whale, size=b, replace=TRUE),
                           w.scale = sample(w.whale, size=b, replace=TRUE)) %>%
                mutate(w.whale = l.whale * w.scale,
                       hdg_start = sample(1:360, size=b, replace=TRUE),
                       hdg_sd = sample(delta.sd, size=b, replace=TRUE),
                       x_start = whale_starts$X,
                       y_start = whale_starts$Y)

              params.whale %>% head

              # Create whale tracks ==============================================

              suppressMessages({
                twhales <-
                  params.whale %>%
                  # Create minutes column
                  group_by(id_whale, v.whale, l.whale, w.scale, w.whale,
                           hdg_start, hdg_sd, x_start, y_start) %>%
                  summarize(t = 1:tmax) %>%
                  mutate(minutes = floor(t/60)) %>%
                  ungroup() %>%
                  # Determine heading changes in each minute
                  group_by(id_whale, minutes) %>%
                  mutate(hdg_change = rnorm(1, mean=0, sd=sample(delta.sd, size=1))) %>%
                  mutate(hdg_to_count = c(hdg_change[1], rep(0, times=(n()-1)))) %>%
                  ungroup() %>%
                  # Determine net heading change in each second
                  group_by(id_whale) %>%
                  mutate(hdg_net_change = cumsum(hdg_to_count)) %>%
                  # Calculate actual heading in each second
                  mutate(hdg_whale = (hdg_start[1] + hdg_net_change) %% 360) %>%
                  mutate(hdg_rad = (pi / 180) * hdg_whale) %>%
                  # Use heading to calculate change in position in each second
                  mutate(dy = sin(hdg_rad + pi/2) * v.whale[1],
                         dx = cos(2*pi - hdg_rad + pi/2) * v.whale[1]) %>%
                  # Use cumulative position changes to calculate actual position
                  mutate(dx_cum = cumsum(dx),
                         dy_cum = cumsum(dy)) %>%
                  mutate(x_rostrum = x_start[1] + dx_cum,
                         y_rostrum = y_start[1] + dy_cum) %>%
                  select(- c('hdg_change', 'hdg_to_count', 'hdg_net_change')) %>%
                  group_split()
              })

              # Combine ship & whale datasets by t ===============================

              #ships[[1]] %>% head
              #twhales[[1]] %>% head

              (shipbig <- ships %>% bind_rows) %>% nrow
              (whalebig <- twhales %>% bind_rows) %>% nrow

              mr <-
                left_join(shipbig, whalebig,
                          by=c('id_ship' = 'id_whale', 't'))

              # Add details on relative position of each ship and whale
              mr <-
                mr %>%
                mutate(x_diff = x_ship - x_rostrum,
                       bow_diff = y_bow - y_rostrum,
                       ctr_diff = y_center - y_rostrum,
                       stern_diff = y_stern - y_rostrum,
                       bow_danger_zone = y_bow + 2*l.whale,
                       stern_danger_zone = y_stern - 2*l.whale,
                       beam_danger_zone = w.ship + 2*l.whale) %>%
                mutate(dist_bow = sqrt((x_diff^2 + bow_diff^2))) %>%
                mutate(dist_center = sqrt((x_diff^2 + ctr_diff^2))) %>%
                mutate(dist_stern = sqrt((x_diff^2 + stern_diff^2))) %>%
                mutate(beam_test = abs(x_diff) <= beam_danger_zone,
                       bow_test = y_rostrum <= bow_danger_zone,
                       stern_test = y_rostrum >= stern_danger_zone)

              mr$danger_zone <-
                apply(mr %>% select(beam_test, bow_test, stern_test), 1, all)
              mr$danger_zone %>% table

              names(mr)

              # Filter to near encounters
              mri <- mr %>% filter(danger_zone == TRUE)
              nrow(mri)

              # Create ellipses for near encounters  ===========================
              # and test for actual close encounters

              if(nrow(mri) > 0){

                # Analyze
                transit_results <- transit_ellipses(mri)

                # Harvest results
                (nenc <- transit_results$n_encounters)
                (ids <- transit_results$encounter_transits)
                transits <- transit_results$transits

                # Join to full dataset
                transits %>% head
                transjoin <- transits %>% select(id_ship, t, nearest, encounter)
                mr  <- left_join(mr, transjoin, by=c('id_ship','t'))
                names(mr)
              }else{
                mr$nearest <- NA
                mr$encounter <- FALSE
              }

              # Review
              mr$encounter %>% table
              nenc
              ids

              # Quick plot  ======================================================
              if(toplot){
                p <-
                  ggplot(arena) +
                  geom_sf(alpha=.3) +
                  theme_light() +
                  geom_vline(xintercept = mr$x_port[1], lty=3, color='darkblue', alpha=.5) +
                  geom_vline(xintercept = mr$x_star[1], lty=3, color='darkblue', alpha=.5) +
                  geom_point(data=mr %>% dplyr::filter(id_ship == 1),
                             aes(x=x_ship, y=y_bow), alpha=.5, color='darkblue', size=.5) +
                  geom_point(data=mr,
                             aes(x=x_rostrum, y=y_rostrum, group=id_ship), alpha=.05, size=.1) +
                  geom_point(data=mr %>% filter(t == 1),
                             aes(x=x_rostrum, y=y_rostrum, group=id_ship), alpha=.4, size=.4) +
                  xlab(NULL) + ylab(NULL) +
                  labs(title = paste0(typi,' : months of ', monthi,' : ',dieli,' : run ', Bi))

                if(length(mri) > 0){
                  p <- p + geom_point(data=mri %>% bind_rows(),
                                      aes(x=x_rostrum, y=y_rostrum, group=id_ship), alpha=.2, size=.4, color='firebrick')
                }
                if(nenc > 0){
                  # Get a df of only the first moment of each encounter
                  encs <-
                    mr %>%
                    filter(encounter == TRUE) %>%
                    group_by(id_ship) %>%
                    filter(nearest == min(nearest, na.rm=TRUE)) %>%
                    filter(row_number() == 1) %>%
                    ungroup()

                  nrow(encs)
                  encs

                  # Add point to plot
                  p <- p + geom_point(data=encs,
                                      aes(x=x_rostrum, y=y_rostrum, group=id_ship), alpha=.9, size=4, color='firebrick')
                }
                print(p)
              }

              # Summarize results ================================================
              (p_encounter <- nenc / b)

              # Add to building dataset
              (resulti <- data.frame(type = typi, month = monthi, diel = dieli, i = Bi, p_encounter))
              results <- rbind(results, resulti)

              # Save copy of data
              (fni <- paste0(outcome_dir, 'p_encounter.RData'))
              #(fni <- paste0(outcome_dir, 'Pencounter_',typi,'.RData'))
              saveRDS(results, file = fni)

              # Add to details dataset
              if(runs == 1){
                mr <- mr %>% mutate(type = typi, month = monthi, diel = dieli)
                MR <- rbind(MR, mr)
              }

              ################################################################
              ################################################################
            } # end of encounters B loop
            message('\nEncounter rates: ',paste(results$p_encounter[results$type %in% c('All', typi) &
                                                                      results$month %in% c('All', monthi) &
                                                                      results$diel %in% c('All', dieli)], collapse=', '))
            par(mfrow=c(1,1))

          } # end of if nrow vmd > 0
        } # end of diels loop
      } # end of if nrow vm > 0
    } # end of month loop
  } # end of type loop

  message('\nFinished!')

  if(runs == 1){
    return(MR)
  }
}
