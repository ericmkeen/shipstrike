################################################################################
# P_encounter rates - FIN WHALES
################################################################################

library(dplyr)
library(ggplot2)
library(devtools)
document()

################################################################################
################################################################################

# Common inputs
whale_source <- 'tests/fw/parameters.R'
runs <- 100
iterations <- 100

# Vessels 2019
vessel_source <- 'tests/vessels_2019.RData'
outcome_dir <- 'tests/fw/impacts_2019/p_encounter/'
encounter_wrapper(vessel_source, whale_source, outcome_dir, runs, iterations)

# LNG Canada
vessel_source <- 'tests/vessels_lng_canada_8_14knots.RData'
outcome_dir <- 'tests/fw/impacts_lng_canada/8_14_knots/p_encounter/'
encounter_wrapper(vessel_source, whale_source, outcome_dir, runs, iterations)

# Cedar LNG
vessel_source <- 'tests/vessels_cedar_lng_8_14knots.RData'
outcome_dir <- 'tests/fw/impacts_cedar_lng/8_14_knots/p_encounter/'
encounter_wrapper(vessel_source, whale_source, outcome_dir, runs, iterations)


################################################################################
################################################################################
