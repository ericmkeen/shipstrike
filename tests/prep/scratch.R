################################################################################
################################################################################
# Strike rates
################################################################################
################################################################################

document() ; load_all()

################################################################################
# Make grid

grids <- make_grid(xlims = c(-129.75, -128.5),
                   ylims = c(52.75, 54.1),
                   grid_int = 1.287)

plot(x=grids$x, y=grids$y, cex=.1, pch=16)
range(grids$km2)
mean(grids$km2)

################################################################################
# Simulate vessels

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

vessels <- simulate_vessel(grids, vessels=list(cfv, tanker))
vessels %>% head

################################################################################
# Interpolate & grid associate vessels
# (this is where you would bring in actual AIS observations)

vgrid <- vessel_grid(grids, vessels)
head(vgrid)
nrow(vgrid)

################################################################################
# Determine sun positions

vgrid$sun <- vessel_sun_angle(vgrid)
vgrid$diel <- 'night'
vgrid$diel[vgrid$sun > 0] <- 'day'
vgrid$diel %>% table

################################################################################
# Summarize grid cells by type, diel, month

vsumm <- summarize_grid(vgrid)
vsumm %>% filter(type == 'tanker') %>% head
nrow(vsumm)

# Speed, length, widht, and draft are saved as collapsed vectors:
(speeds <- stringr::str_split(vsumm$speed[1], pattern='_'))[[1]] %>% as.numeric
(speeds <- stringr::str_split(vsumm$speed[nrow(vsumm)], pattern='_'))[[1]] %>% as.numeric


################################################################################
# Simulate whale density

vsumm <- simulate_whale(vsumm, d_mean = 0.015, d_cv = 0.135)
head(vsumm)
tail(vsumm)


################################################################################
# vsumm is the core dataset

vsumm %>% tail
vsumm %>% nrow

# For each type:

# For each month:

# For each diel period:

# Create p(whale) distribution
    # Draw 10,000 random draws from each grid cell according to its mean / CV
    # Pool through draws to create the p(whale) distribution
pwhale <- truncnorm::rtruncnorm(n=10000,
                      mean = vsumm$d_mean,
                      sd = vsumm$d_cv * vsumm$d_mean,
                      a=0)
hist(pwhale, breaks=20)
length(pwhale)

# Create p(encounter) distribution
    # Draw speed/length/width/draft
    # Draw whale length/speed/track_variability # Get whale width

#b <- 1
#summaries <- data.frame()
#encounter_tally <- c()
B <- 100
b <- 100
pb <- txtProgressBar(1, B, style=3) # setup progress bar
p_encounters <- c()
for(i in 1:B){
  params.ship <- data.frame(v.ship=10, l.ship=300, w.ship=50)
  v.whale <- 2
  l.whale <- 20
  w.whale <- 5
  delta.sd <- 30
  encounters <- encounter_simulator(params.ship=params.ship,
                                    v.whale=v.whale,
                                    l.whale=l.whale,
                                    w.whale=w.whale,
                                    delta.sd=delta.sd,
                                    B=b,
                                    save_records=FALSE,
                                    speedy=TRUE,
                                    verbose=FALSE,
                                    toplot=FALSE)
  (encs <- encounter_tally(encounters)$total)
  (p_encounter <- encs / B)
  p_encounters <- c(p_encounters, p_encounter)
  setTxtProgressBar(pb, i) # update progress bar
}


# Create p(surface) distribution
    # For now, treat as static value

# Create p(avoidance) distribution
    # At first, treat as static value
    # Figure out how to calculate this

# Create p(lethality) distribution
    # Use published data to estimate, based on size/speed
    # Create distribution using size/speed values


# Draw from them a bunch and use the outcome() function (with each outcome call, sum across n grid type/month/diel)


n=10 # say there were n grid cell transits within this type/month/diel

outcome(p_whale = runif(n, 0.001, .99),
        p_encounter = runif(n, 0.001, .99),
        p_surface = runif(n, 0.001, .99),
        p_avoidance = runif(n, 0.001, .99),
        p_lethality = runif(n, 0.001, .99)) %>%
  apply(2,sum)



################################################################################
# Core wrapper: random draws for each parameter, then pass to core function





# For a single year-month:

# vessels
# year, month, grid_id, lat, lon, v_id, v_class, v_time, v_length, v_width, v_draft, v_speed

# whales
# year, month, grid_id, lat, lon, species, d_mean, d_var,

# encounter

# behavior
# year, month, p_day, p_day_var, p_night, p_night_var, avoidance_function, lethality_function






################################################################################
# Bootstrap wrapper: for single distributions (e.g., for a single month)

################################################################################
# Annual wrapper: inputs are distributions for each month, for a single vessel-type and species

#grid_cells # dataframe with x y ID information for all cells considered

################################################################################
# Master wrapper: inputs are annual lists for each vessel type and species




################################################################################
################################################################################
