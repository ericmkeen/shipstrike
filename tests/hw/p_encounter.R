################################################################################
# P_encounter rates - HUMPBACK WHALES
################################################################################

library(dplyr)
library(ggplot2)
library(devtools)
document()

################################################################################
################################################################################

# Common inputs
whale_source <- 'tests/hw/parameters.R'
runs <- 100
iterations <- 100

# Vessels 2019
vessel_source <- 'tests/vessels_2019.RData'
outcome_dir <- 'tests/hw/impacts_2019/p_encounter/'
encounter_wrapper(vessel_source, whale_source, outcome_dir, runs, iterations)

# LNG Canada
vessel_source <- 'tests/vessels_lng_canada_8_14knots.RData'
outcome_dir <- 'tests/hw/impacts_lng_canada/8_14_knots/p_encounter/'
encounter_wrapper(vessel_source, whale_source, outcome_dir,
                  process_months = FALSE, runs, iterations)

# Cedar LNG
vessel_source <- 'tests/vessels_cedar_lng_8_14knots.RData'
outcome_dir <- 'tests/hw/impacts_cedar_lng/8_14_knots/p_encounter/'
process_months <- FALSE
encounter_wrapper(vessel_source, whale_source, outcome_dir,
                  process_months = FALSE, runs, iterations)


################################################################################
################################################################################
