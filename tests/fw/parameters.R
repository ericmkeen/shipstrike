################################################################################
# Parameters -- Fin whale
################################################################################

# Whale dimensions
# will be drawn from a truncated normal distribution
# source: Keen et al 2021
length_min <- 10
length_max <- 26
length_mean <- 20
length_sd <- 1.65
width_factor <- 0.2074 # will be used to scale length to find width

# Whale speed
# will be drawn from a truncated normal distribution
# source: Hendricks et al 2021
# speed_day_min <- 0
# speed_day_max <- 2.6
# speed_day_mean <- 1.36
# speed_day_sd <- 0.5

# speed_night_min <- 0
# speed_night_max <- 2.6
# speed_night_mean <- 1.36
# speed_night_sd <- 0.5

# New numbers
speed_day_min <- 0.27
speed_day_max <- 2.22
speed_day_mean <- 1.140301
speed_day_sd <- 0.4375298

speed_night_min <- 0.55
speed_night_max <- 3.04
speed_night_mean <- 1.809226
speed_night_sd <- 0.5718535

# Whale track variability
# defined as the standard deviation of changes in whale course every 60 seconds;
# values will be drawn from a normal distribution then reverse-log-transformed
# source: Hendricks et al. 2021
#delta_day_mean <- 1.0424
#delta_day_sd <- 0.82477

#delta_night_mean <- 1.0424
#delta_night_sd <- 0.82477

# New numbers
# day n = 28
delta_day_mean <- 0.8087532
delta_day_sd <- 1.332026

# day n = 61
delta_night_mean <- 1.494988
delta_night_sd <- 1.386166


################################################################################
