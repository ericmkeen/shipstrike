################################################################################
# Parameters -- Humpback whale
################################################################################

# Whale dimensions
# will be drawn from a truncated normal distribution
#gregr et al 2000 whaling data
# for mean, take avearge of male and female
(length_mean = mean(c(39.1, 40.4)) * 0.3048) # m , f
# for max, use female max
(length_max = 51 * 0.3048)
# for min and sd, approximate normal distribution such that only 2.35% exceeds min and max respectively
(length_min = 28 * 0.3048)
(length_sd = 5 * 0.3048)

# How those were determined:
if(FALSE){
  (lmean <-  mean(c(39.1, 40.4)))
  x <- rnorm(10000, mean= lmean, sd=5)
  hist(x, breaks=20, xlim=c(20, 60)) ; abline(v=lmean, col='red', lwd=2)
  abline(v=51, col='steelblue4', lwd=3)
  lci <- quantile(x, .01) ; abline(v=lci, col='red',lty=3)
  uci <- quantile(x, .99) ; abline(v=uci, col='red',lty=3) ; uci
}

# rockwood et al 2017 uses these width and lengths, which we will use to develop a scaling factor
(width_factor <- 3.21 / 13.5)


# Whale speed (meters / second)
# will be drawn from a truncated normal distribution

# daytime ARS travel speed (drawon from Rockwood et al. 2017)
speed_day_mean <- 0.7111 # 2.56 kmh
speed_day_sd <- 0.20833 # 0.75 kmh
speed_day_max <- 1.166 # 4.2 kmh # mean traveling rate from Rockwood
speed_day_min <- 0.13888 # equal to 0.5 kmh (arbitrary)

# Ratios from Fin Whales
# source: Hendricks et al 2021 -- using fin whale numbers for now
fw_speed_day_mean <- 1.140301
fw_speed_night_mean <- 1.809226
fw_speed_night_sd <- 0.5718535
(fw_diel_ratio <- fw_speed_night_mean / fw_speed_day_mean)
(fw_night_cv <- fw_speed_night_sd / fw_speed_night_mean)

(speed_night_mean <- speed_day_mean * fw_diel_ratio)
(speed_night_min <- speed_day_min * fw_diel_ratio)
(speed_night_max <- speed_day_max * fw_diel_ratio)
(speed_night_sd <- speed_day_mean * fw_night_cv)

# Whale track variability
# use same as Fin whales # source: Hendricks et al. 2021
delta_day_mean <- 0.8087532
delta_day_sd <- 1.332026
delta_night_mean <- 1.494988
delta_night_sd <- 1.386166


################################################################################
