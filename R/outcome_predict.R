#' Predict whale-ship interaction outcomes
#'
#' This is the primary function for ship-strike analysis in the `shipstrike` package.
#'
#' @param traffic A `data.frame` of vessel position fixes. Each row ought to be
#' a position fix within a spatial grid cell for a single transit from a single vessel,
#' with no more than one row per cell-transit-vessel.  See `data(ais_2019)` for an example.
#' Required fields:
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
#' \item `channel` = Waterway or channel (i.e., a geographic subgreion of your study area)
#' }
#'
#' @param scale_factors Optional. A `data.frame` containing scaling factors to use
#' for each vessel type within `traffic`. The number of transits within a grid cell will be scaled
#' by this factor, which can be helpful if you are extrapolating a future traffic scenario
#' based on type-specific trends. This is Keen et al. (2023) used
#'  2019 traffic data to predict 2030 AIS traffic. The required fields for this `data.frame` are
#'  `type` and `scale_factor`, where 1.0 means no change in the number of transits.
#'
#' @param whale Spatial grid of whale density (i.e., a density surface). This
#' spatial grid needs to be the same one used to summarize vessel traffic. It requires three fields:
#' `grid_id` (the grid cell id), 'iteration' (the bootstrap iteration), and `D` (the density estimate).
#' This `data.frame` may optionally have a field named `month`.
#' If you have only one density surface for the entire year, you can set `month` to `0`.
#' If you don't have a density surface available but you want to explore `shipstrike` functionality,
#' try the `simulate_whale()` function.
#'
#' @param seasonal Optional: a seasonal abundance curve used to scale the
#' density surface in each month. This can be useful if your density surface has the
#' same distribution in all months, but the overall abundance changes over time,
#' as with the case of fin whales in Keen et al. (2023).
#' This `data.frame` requires three fields:
#' `iteration` (the bootstrap iteration that produced this iteration of the curve,
#' which offers a means of incorporating uncertainty),
#' `month`, and `scale_factor` (a number above 0 used to scale the density of each spatial
#' grid cell; a value of 1 means no change to the grid cell's density).
#'
#' @param p_encounter_dir Can be left as `NULL` if the results of `encounter_rate()` are stored in the same
#' `outcome_dir` specified below.  Otherwise, specify a different path here. See the
#' `encounter_rate()` function for details on what is expected here.
#'
#' @param surface The whale depth distribution,
#' indicating the proportion of time spent at various depths, including the surface.
#' An example is provided in the `shipstrike` dataset `data(p_surface)`.
#' Required fields of this `data.frame` are `diel` (character indicating `"day"` or `"night"`),
#' `z` (depth, in meters), `p_mean` (the average fraction of time spent at or above this depth),
#' and `p_sd` (the SD of that fraction).
#'
#' @param avoidance Model parameters for the logistic relationship between P(Collision)
#' and vessel speed: we provide the model parameters used in Keen et al. (2023)
#' as a `shipstrike` dataset `data(p_collision)`. This `data.frame` needs 4 fields:
#' `type` (vessel type), `c1` (model parameter 1), `c2` (model parameter 2), and `asymptote`.
#' See the `shipstrike` function `collision_curve()` for further details.
#'
#' @param lethality Model parameters for the logistic relationship between P(Lethality)
#' and vessel speed: we provide the model parameters used in Keen et al. (2023)
#' as a `shipstrike` dataset `data(p_lethality)`. This `data.frame` needs 4 fields:
#' `type` (vessel type), `c1` (model parameter 1), `c2` (model parameter 2), and `asymptote`.
#' See the `shipstrike` function `lethality_curve()` for further details.
#'
#' @param outcome_dir Path specifying the directory into which the simulator result will be saved.
#' Default is your working directory.
#'
#' @param asymptote_scaling An option to scale asymptote of the P(Collision) curve above to
#' readily/cheaply explore the effects of changing collision~speed relationships on ship-strike
#' results. A vaue of 1 means no change in the asymptote. If the asymptote is 0.9 and `asymptote_scaling`
#' is 0.5, the new asymptote would be 0.45.
#'
#' @param species A character specifying the species (or any other label you wish to apply) to
#' which this analysis pertains. Will be added to results in order to stave off confusion with other
#' results from other runs.
#'
#' @param year The year pertaining to this analysis. Will be added to the results objects.
#'
#' @param iterations The number of iterations to use to produce the posterior distributions of each outcome estimate.
#' Default is 1,000.
#'
#' @return The outcome of this function is two large `data.frames` of results saved as
#' `outcomes.RData` (spatially summarized results for each iteration) and
#' `outcomes_grid.RData` (spatially explicit grid of outcomes summed across iterations)
#' inside `outcome_dir`.
#'
#' The `outcomes.Rdata` table contains a row for each iteration, with the following fields:
#' \itemize{
#' \item `species` = The same `species` provided as an input.
#' \item `year` = The same `year` provided as an input.
#' \item `channel` = Waterway for which outcomes are being summed across the spatial grid therein.
#' \item `vessel` = Vessel type for which outcomes are being summed.
#' \item `month` = Month in which outcomes are being summed.
#' \item `diel` = Diel period in which outcomes are being summed.
#' \item `iteration` = Iteration for which otucomes are being summed.
#' \item `cooccurrence` = Number of cooccurrences predicted for this iteraiton of the channel-vessel-month-diel scenario.
#' \item `encounter` = Number of close-encounter events predicted (see `encounter_rate()`)
#' \item `surface` = Number of strike zone events predicted, in which the strike zone is 1x vessel draft.
#' \item `surface2` = Strike zone is 1.5x vessel draft.
#' \item `collision1.1` = Collisions predicted, where strike zone is 1x draft and P(Collision) is a constant 0.55.
#' \item `collision1.2` = P(Collision) is a function of vessel speed according to `avoidance` input.
#' \item `collision1.3` = Deprecated (will return same as `collision1.2`)
#' \item `collision1.4` = P(Collision) is 1.0 (i.e., no avoidance.)
#' \item `collision2.1` = Same as above, but strike zone is 1.5x vessel draft.
#' \item `collision2.2`
#' \item `collision2.3`
#' \item `collision2.4`
#' \item `mortality1.1` = Mortalities predicted (strike zone and P(Collision) numbers follow pattern above.)
#' \item `mortality1.2`
#' \item `mortality1.3`
#' \item `mortality1.4`
#' \item `mortality2.1`
#' \item `mortality2.2`
#' \item `mortality2.3`
#' \item `mortality2.4`
#' }
#'
#' Pass these results on to other `outcome_` functions in `shipstrike`, such as `outcome_table()` and
#' `outcome_histograms()`. See the vignette for further details.
#'
#' The `outcomes_grid.RData` table contains a row for grid cell,
#' summed across all iterations, with the same fields as above except with `grid_cell`
#' instead of `iteration`. Pass this result grid on to `outcome_map()`.
#'
#'
#' @export
#'
outcome_predict <- function(traffic,
                             scale_factors = NULL,
                             whale,
                             seasonal = NULL,
                             p_encounter_dir = NULL,
                             surface,
                             avoidance,
                             lethality,
                             outcome_dir = '',
                             asymptote_scaling = NULL,
                             month_batches = list(winter = c(0:4, 11:12),
                                                  summer = 5:10),
                             species = 'fw',
                             year = 2019,
                             iterations = 1000){

  ################################################################################
  if(FALSE){
    # Traffic
    data(lng_canada)
    traffic <- lng_canada
    traffic %>% head

    outcome_dir <- 'tests/fw/impacts_lng_canada/8_14_knots/'
    scale_factors = NULL
    asymptote_scaling = NULL

    # Whale density bootstraps
    load('tests/fw/dsm-bootstraps.RData')
    whale <- bootstraps
    whale %>% head
    whale$month %>% table

    # Seasonal posterior
    load('tests/fw/seasonal_posterior.RData')
    seasonal <- seasonal_boot
    #seasonal %>% names

    # Encounter rate
    p_encounter_dir <- NULL

    # P surface (depth distribution)
    data(p_surface)
    surf <- p_surface
    surf %>% head

    # Avoidance
    data(p_collision)
    avoidance <- p_collision
    avoidance
    (avoidance <- avoid[c(2,4),])
    (avoidance$type <- c(paste(vessels[3:4], collapse=' | '),
                         paste(vessels[1:2], collapse=' | ')))
    avoidance

    # Lethality
    data(p_lethality)
    lethality <- p_lethality
    lethality
    (lethality <- lethality[c(2,4),])
    (lethality$type <- c(paste(vessels[3:4], collapse=' | '),
                         paste(vessels[1:2], collapse=' | ')))
    lethality

    month_batches = list(winter = c(0:4, 11:12),
                         summer = 5:10)
    scale_factors = NULL
    species = 'fw'
    year=2030
    iterations = 1000
  }

  ################################################################################
  # Load built-in datasets
  data(grid_kfs)
  head(grid_kfs)
  data(p_covered)
  kms <- p_covered

  # Rename for convenience
  boots <- whale
  avoid <- avoidance
  lethal <- lethality
  surf <- surface

  # Determine months available in dataset
  (months_in_data <- unique(boots$month))
  (months_available <- months_in_data[months_in_data > 0])

  # Encounter rate file list
  if(is.null(p_encounter_dir)){p_encounter_dir <- outcome_dir}
  (penc_file <- paste0(p_encounter_dir,'p_encounter.RData'))
  pencounter_master <- readRDS(penc_file)

  # Make sure outcome dir exists
  #(outcome_dir <- paste0(outcome_dir,'outcomes/'))
  #if(!dir.exists(outcome_dir)){
  #  dir.create(outcome_dir)
  #}

  # Other prep
  (vessels <- unique(traffic$type))
  (channels <- unique(traffic$channel))

  ################################################################################

  # for debugging
  months <- 1:12
  diels <- c('day','night')
  chi <- 1
  vi <- 1
  mi <- 1
  di <- 1

  # stage results arrays
  MRS <- data.frame()
  (fn <- paste0(outcome_dir,'outcomes.RData'))

  GRID <- data.frame()
  (fn_grid <- paste0(outcome_dir,'outcomes_grid.RData'))


  for(chi in 1:length(channels)){
    (channi <- channels[chi])

    for(vi in 1:length(vessels)){
      (vessi <- vessels[vi])

      for(mi in 1:length(months)){
        (monthi <- months[mi])
        month_batches
        batchi <- lapply(month_batches, function(x){monthi %in% x}) %>% unlist %>% which
        (month_batchi <- names(month_batches)[batchi])

        for(di in 1:length(diels)){
          (dieli <- diels[di])

          ######################################################################

          (traffi <-
             traffic %>%
             dplyr::filter(channel == channi,
                           type == vessi,
                           month == monthi,
                           diel == dieli)) %>% head

          scale_factor <- 1
          if(!is.null(scale_factors)){
            (sfi <- scale_factors %>% dplyr::filter(type == vessi))
            if(nrow(sfi)>0){
              scale_factor <- sfi$scale_factor
            }else{
              stop('This vessel type was not found in the `scale_factors` dataframe!')
            }
          }
          scale_factor

          # Begin iterations  ====================================================
          message('\n========== Channel ',channi,' :: Vessel type ',vessi,' :: Month ',monthi,' :: Diel period ',dieli,' ==========\n')

          mr <- data.frame()

          # Get scenario-specific probabilities   ================================

          # Handle month-specific whale density surfaces
          booti <- boots
          if(length(unique(boots$month))>1){
            # If there are multiple months in the whale grid, this is a humpback distribution
            # Use the generic average grid or the month-specific grid?
            if(monthi %in% months_available){
              booti <- boots %>% dplyr::filter(month == monthi)
            }else{
              # There is no specific grid for this month. Just using the generic grid
              booti <- boots %>% dplyr::filter(month == 0)
            }
          }
          booti %>% nrow

          p_seasonal <- c(1,1,1)
          if(!is.null(seasonal)){
            (p_seasonal <- seasonal %>% dplyr::filter(month == monthi)) %>% head
            p_seasonal <- p_seasonal$scaled
          }
          p_seasonal %>% mean
          p_seasonal %>% median

          (p_encounter <-
              pencounter_master %>%
              dplyr::filter(type == vessi,
                            diel == dieli) ) %>% head

          if(nrow(p_encounter) > 0 &
             month_batchi %in% unique(p_encounter$month)
             ){
              p_encounter <- p_encounter %>% dplyr::filter(month == month_batchi)
          }
          p_encounter %>% head

          (p_surface <- surf %>% dplyr::filter(diel == dieli)) %>% head
          (p_avoid <- avoid[grep(vessi, avoid$type) , ])
          (p_lethal <- lethal[grep(vessi, lethal$type) , ])

          # Co-occurrence ======================================================
          message('------ co-occurrences ...')

          if(nrow(traffi) > 0 & scale_factor > 0){

            traffi %>% head
            traffi %>% nrow

            # Get grid cells transited
            (grid_cells <- traffi$grid_id)
            length(grid_cells)

            # Handle scale factor
            scale_factor
            (new_grid_n <- round(length(grid_cells) * scale_factor))
            if(scale_factor > 1){
              (new_cells_needed <- new_grid_n - length(grid_cells))
              if(length(new_cells_needed)>0){
                (new_cells <- sample(grid_cells, size=new_cells_needed, replace=TRUE))
                grid_cells <- c(grid_cells, new_cells)
              }
            }
            if(scale_factor < 1){
              (n_cells_to_remove <- length(grid_cells) - new_grid_n)
              if(length(n_cells_to_remove)>0){
                (bad_cells <- sample(1:length(grid_cells), size=n_cells_to_remove, replace=TRUE))
                grid_cells <- grid_cells[- bad_cells]
              }
            }
            length(grid_cells)

            if(length(grid_cells)>1){

              suppressMessages({
                # Create a bootstrapped dataset, with 1000 rows for each grid cell, each with resampled density
                # Stage resulting dataframe
                mri <- data.frame(grid_id = rep(grid_cells, each=iterations),
                                  iteration = rep(1:iterations, times=length(grid_cells)),
                                  dwhale = 0)

                # Filter density bootstraps to the grid cells of interest
                bootif <- booti %>% filter(grid_id %in% grid_cells)

                # Resample those bootstraps
                booti_sampled <-
                  bootif %>%
                  group_by(grid_id) %>%
                  summarize(iteration = 1:iterations,
                            dwhale = sample(D, size = iterations, replace=TRUE))

                # Join the two datasets & format
                mri <- dplyr::left_join(mri, booti_sampled, by=c('grid_id', 'iteration'))
                mr <-
                  mri %>%
                  mutate(d_whale = ifelse(dwhale.y > 0, dwhale.y, dwhale.x)) %>%  # treat negatives as 0
                  select(grid_id, iteration, d_whale)

                # Review
                mr %>% head
              })

            }else{
              mr <- data.frame(grid_id = grid_cells[1], iteration = 1:iterations, d_whale = -1)
            }
          }else{
            message('------   No vessel traffic for this channel-type-month-diel scenario!')
            mr <- data.frame(grid_id = NA, iteration = 1:iterations, d_whale = -1)
          }

          # Scale by seasonal abundance
          mr$seasonal <- sample(p_seasonal, size=nrow(mr), replace=TRUE)

          # Calculate p_whale
          mr$p_whale <- mr$d_whale * mr$seasonal

          # Test if whale is present
          mr$pwr <- runif(0,1,n=nrow(mr))
          mr$cooccur <- 0
          mr$cooccur[mr$p_whale > mr$pwr] <- 1
          mr$cooccur %>% table
          mr %>% head

          # Rescind some of these coccurrences based on the proportion of the
          # km2 grid that was actually covered.
          mr$p_covered <- sample(kms, size=nrow(mr), replace=TRUE) # randomly sample prop of km covered
          mr$pcovr <- runif(0, 1, n=nrow(mr)) # compare to a random draw
          (bads <- which(mr$cooccur == 1 & mr$p_covered < mr$pcovr)) %>% length # which need to be rescinded
          mr$cooccur[bads] <- 0 # rescind
          mr$cooccur %>% table # check effect


          # Close encounter  =====================================================
          message('------ close encounters ...')

          mr$penc <- -1
          mr$pencr <- runif(0,1,n=nrow(mr))
          mr$enc <- 0
          if(nrow(p_encounter)>0){
            p_encounter %>% head
            mr$penc <- sample(p_encounter$p_encounter, size=nrow(mr), replace=TRUE)
            mr$enc[mr$cooccur == 1 & mr$penc > mr$pencr] <- 1
            mr$enc %>% table
            mr$enc[mr$cooccur == 1] %>% table
            mr %>% dplyr::filter(cooccur == 1)
          }else{
            message('------   No p_encounter data for this channel-type-month-diel scenario!')
          }

          # Strike zone events 1 =================================================
          message('------ strike zone potentiality 1: draft == strike zone ...')

          if(nrow(traffi)>0){
            (drafts <- traffi$draft)
          }else{
            drafts <- c(0,0,0)
          }
          drafts

          mr$draft <- sample(drafts, size=nrow(mr), replace=TRUE)
          mr$pdraft <- rep(-1, times=nrow(mr))
          mr$pdraftr <- runif(0,1,n=nrow(mr))
          mr$surf <- 0
          (testi <- which(mr$enc == 1))
          if(length(testi)>0){
            i=1
            for(i in 1:length(testi)){
              (testii <- testi[i])
              (drafti <- round(mr$draft[testii]))
              (p_surface_mean <- p_surface$p_mean[p_surface$z == drafti])
              (p_surface_sd <- p_surface$p_sd[p_surface$z == drafti])
              (p_surfi <- truncnorm::rtruncnorm(n=1,
                                                a = 0,
                                                b=1,
                                                mean=p_surface_mean,
                                                sd=p_surface_sd))
              mr$pdraft[testii] <- p_surfi
            }
            mr$pdraft %>% table
            mr$surf[mr$enc == 1 & mr$pdraft > mr$pdraftr] <- 1
          }
          mr$surf %>% table
          mr %>% head

          # Strike zone events 2 ==================================================
          message('------ strike zone potentiality 2: draft x 1.5 == strike zone ...')

          if(nrow(traffi)>0){
            (drafts <- traffi$draft) * 1.5
          }else{
            drafts <- c(0,0,0)
          }
          drafts

          mr$draft2 <- sample(drafts, size=nrow(mr), replace=TRUE)
          mr$pdraft2 <- rep(-1, times=nrow(mr))
          mr$pdraftr2 <- runif(0,1,n=nrow(mr))
          mr$surf2 <- 0
          (testi <- which(mr$enc == 1))
          if(length(testi)>0){
            i=1
            for(i in 1:length(testi)){
              (testii <- testi[i])
              (drafti <- round(mr$draft2[testii]))
              (p_surface_mean <- p_surface$p_mean[p_surface$z == drafti])
              (p_surface_sd <- p_surface$p_sd[p_surface$z == drafti])
              (p_surfi <- truncnorm::rtruncnorm(n=1,
                                                a = 0,
                                                b=1,
                                                mean=p_surface_mean,
                                                sd=p_surface_sd))
              mr$pdraft2[testii] <- p_surfi
            }
            mr$pdraft2 %>% table
            mr$surf2[mr$enc == 1 & mr$pdraft2 > mr$pdraftr2] <- 1
          }
          mr$surf2 %>% table
          mr %>% head

          #  Collisions  1  =======================================================
          message('------ collisions (scenario 1: constant P(Avoidance) of 0.55) ...')

          mr$p_collision1 <- 1 - 0.55
          mr$collider1 <- runif(0,1,n=nrow(mr))

          mr$collide1.1 <- 0
          mr$collide1.1[mr$surf == 1 & mr$p_collision1 > mr$collider1] <- 1
          mr$collide1.1 %>% table

          mr$collide2.1 <- 0
          mr$collide2.1[mr$surf2 == 1 & mr$p_collision1 > mr$collider1] <- 1
          mr$collide2.1 %>% table

          #  Collisions  2 =======================================================
          message('------ collisions (scenario 2: avoidance ~ speed) ...')

          head(mr)
          if(nrow(traffi)>0){
            speeds <- traffi$speed
          }else{
            speeds <- c(0,0,0)
          }
          speeds

          mr$speed <- sample(speeds, size=nrow(mr), replace = TRUE)
          #speeds <- seq(0,15,length=1000)
          #collides <-  (1 / (1 + exp(-0.5*(speeds - 11.8))))
          #plot(collides~speeds)

          mr$p_collision2 <- p_avoid$asymptote / (1 + exp(p_avoid$c1[1] *(mr$speed - p_avoid$c2[1])))
          #hist(mr$p_collision2)
          mr$collider2 <- runif(0,1,n=nrow(mr))

          mr$collide1.2 <- 0
          mr$collide1.2[mr$surf == 1 & mr$p_collision2 > mr$collider2] <- 1
          mr$collide1.2 %>% table

          mr$collide2.2 <- 0
          mr$collide2.2[mr$surf2 == 1 & mr$p_collision2 > mr$collider2] <- 1
          mr$collide2.2 %>% table


          #  Collisions  3 =======================================================
          message('------ collisions (scenario 3: scaled asymptote) ...')

          # allow manual scaling of asymptote, if specified
          if(is.null(asymptote_scaling)){
            new_asymptote <- p_avoid$asymptote
          }else{
            new_asymptote <- p_avoid$asymptote * asymptote_scaling
          }

          mr$p_collision3 <- new_asymptote / (1 + exp(p_avoid$c1[1] *(mr$speed - p_avoid$c2[1])))
          mr$collider3 <- runif(0,1,n=nrow(mr))

          mr$collide1.3 <- 0
          mr$collide1.3[mr$surf == 1 & mr$p_collision3 > mr$collider3] <- 1
          mr$collide1.3 %>% table

          mr$collide2.3 <- 0
          mr$collide2.3[mr$surf2 == 1 & mr$p_collision3 > mr$collider3] <- 1
          mr$collide2.3 %>% table

          #  Collisions  4 =======================================================
          message('------ collisions (scenario 4: no avoidance) ...')

          mr$collide1.4 <- mr$surf
          mr$collide2.4 <- mr$surf2

          # Mortalities  =========================================================
          message('------ mortalities ...')

          #(speeds <- unique(mr$speed))
          #morts <-  1 / (1 + exp(-1*(p_lethal$c1[1]+ (p_lethal$c2[1]*speeds))))
          #plot(morts~speeds, xlim=c(0,30), ylim=c(0,1))
          p_lethal

          mr$p_lethality <- p_lethal$asymptote / (1 + exp(-1*(p_lethal$c1[1]+ (p_lethal$c2[1]*mr$speed))))
          #mr$p_lethality %>% unique
          #plot(mr$p_lethality~mr$speed, xlim=c(0,30), ylim=c(0,1))
          mr$mortr <- runif(0,1,n=nrow(mr))

          mr$mort1.1 <- 0
          mr$mort1.1[mr$collide1.1 == 1 & mr$p_lethality > mr$mortr] <- 1
          mr$mort1.2 <- 0
          mr$mort1.2[mr$collide1.2 == 1 & mr$p_lethality > mr$mortr] <- 1
          mr$mort1.3 <- 0
          mr$mort1.3[mr$collide1.3 == 1 & mr$p_lethality > mr$mortr] <- 1
          mr$mort1.4 <- 0
          mr$mort1.4[mr$collide1.4 == 1 & mr$p_lethality > mr$mortr] <- 1

          mr$mort2.1 <- 0
          mr$mort2.1[mr$collide2.1 == 1 & mr$p_lethality > mr$mortr] <- 1
          mr$mort2.2 <- 0
          mr$mort2.2[mr$collide2.2 == 1 & mr$p_lethality > mr$mortr] <- 1
          mr$mort2.3 <- 0
          mr$mort2.3[mr$collide2.3 == 1 & mr$p_lethality > mr$mortr] <- 1
          mr$mort2.4 <- 0
          mr$mort2.4[mr$collide2.4 == 1 & mr$p_lethality > mr$mortr] <- 1

          # Summarize grid  ====================================================

          message('------ summarizing spatial grid across iterations ...')
          mr %>% head
          mr %>% nrow
          mrgrid <-
            mr %>%
            dplyr::group_by(grid_id) %>%
            dplyr::summarize(cooccurrence = sum(cooccur),
                             encounter = sum(enc),
                             surface = sum(surf),
                             surface2 = sum(surf2),
                             collision1.1 = sum(collide1.1),
                             collision1.2 = sum(collide1.2),
                             collision1.3 = sum(collide1.3),
                             collision1.4 = sum(collide1.4),
                             collision2.1 = sum(collide2.1),
                             collision2.2 = sum(collide2.2),
                             collision2.3 = sum(collide2.3),
                             collision2.4 = sum(collide2.4),
                             mortality1.1 = sum(mort1.1),
                             mortality1.2 = sum(mort1.2),
                             mortality1.3 = sum(mort1.3),
                             mortality1.4 = sum(mort1.4),
                             mortality2.1 = sum(mort2.1),
                             mortality2.2 = sum(mort2.2),
                             mortality2.3 = sum(mort2.3),
                             mortality2.4 = sum(mort2.4))

          # Add scenario details
          mrgrid <- data.frame(species = species,
                               year = year,
                               channel = channi,
                               vessel = vessi,
                               month = monthi,
                               diel = dieli,
                               mrgrid)

          mrgrid %>% nrow
          mrgrid %>% head

          # Summarize iterations ===============================================

          message('------ summarizing across spatial grid for each iteration ...')

          mrs <-
            mr %>%
            dplyr::group_by(iteration) %>%
            dplyr::summarize(cooccurrence = sum(cooccur),
                             encounter = sum(enc),
                             surface = sum(surf),
                             surface2 = sum(surf2),
                             collision1.1 = sum(collide1.1),
                             collision1.2 = sum(collide1.2),
                             collision1.3 = sum(collide1.3),
                             collision1.4 = sum(collide1.4),
                             collision2.1 = sum(collide2.1),
                             collision2.2 = sum(collide2.2),
                             collision2.3 = sum(collide2.3),
                             collision2.4 = sum(collide2.4),
                             mortality1.1 = sum(mort1.1),
                             mortality1.2 = sum(mort1.2),
                             mortality1.3 = sum(mort1.3),
                             mortality1.4 = sum(mort1.4),
                             mortality2.1 = sum(mort2.1),
                             mortality2.2 = sum(mort2.2),
                             mortality2.3 = sum(mort2.3),
                             mortality2.4 = sum(mort2.4))

          # Add scenario details
          mrs <- data.frame(species = species,
                            year = year,
                            channel = channi,
                            vessel = vessi,
                            month = monthi,
                            diel = dieli,
                            mrs)

          mrs %>% head
          nrow(mrs)

          # Diagnostics ========================================================
          if(FALSE){
            mr %>% head
            mr$grid_id %>% unique %>% length

            mr %>%
              select(d_whale:p_whale,
                     penc, draft, pdraft,
                     p_collision2,
                     p_lethality) %>%
              summarize(across(d_whale:p_lethality, mean)) %>% t %>% round(4)
          }

          # Adding to results ==================================================
          MRS <- rbind(MRS, mrs)
          GRID <- rbind(GRID, mrgrid)

          ######################################################################

        } # end of diel periods
      }# end of months

      # Save to file   =====================================================
      message('------ saving results ...')
      saveRDS(MRS, file=fn)
      saveRDS(GRID, file=fn_grid)

    } # end of vessel types
  } # end of channels

  message('\nFinished!')
}
