################################################################################
# Outcomes analyses - FIN WHALES
################################################################################

library(dplyr)
library(ggplot)
library(devtools)
document()

################################################################################
################################################################################
# Common datasets to all analyses

# Whale density bootstraps
load('tests/fw/dsm-bootstraps.RData')
whale <- bootstraps

# Seasonal posterior
load('tests/fw/seasonal_posterior.RData')
seasonal <- seasonal_boot

# P surface (depth distribution)
surface <- readRDS('tests/fw/p_surface.RData')


################################################################################
################################################################################
# 2019 traffic

# Prep datasets ================================================================

# Traffic
traffic <- readRDS('tests/vessels_2019.RData')

# Encounter rate
p_encounter_dir <- 'tests/fw/impacts_2019/p_encounter/'

# Output directory
outcome_dir <- 'tests/fw/impacts_2019/outcomes/'

# Avoidance
avoidance <- readRDS('tests/p_avoidance.RData')
avoidance

# Lethality
lethality <- readRDS('tests/p_lethal.RData')
lethality

# Run outcome predictions ======================================================

predict_outcomes(traffic,
                 whale,
                 seasonal,
                 p_encounter_dir,
                 surface,
                 avoidance,
                 lethality,
                 outcome_dir,
                 iterations = 1000)


################################################################################
################################################################################
# LNG Canada - best case (7 - 9 knots, as proposed)

# Prep datasets ================================================================

# Traffic
traffic <- readRDS('tests/vessels_lng_canada.RData')

# Encounter rate
p_encounter_dir <- 'tests/fw/impacts_lng_canada/proposed_speed/p_encounter/'

# Output directory
outcome_dir <- 'tests/fw/impacts_lng_canada/proposed_speed/outcomes/'

# Avoidance
avoidance <- readRDS('tests/p_avoidance.RData')
avoidance
(avoidance <- avoidance[c(2,4),])
(avoidance$type <- c(paste(vessels[3:4], collapse=' | '),
                     paste(vessels[1:2], collapse=' | ')))
avoidance

# Lethality
lethality <- readRDS('tests/p_lethal.RData')
(lethality <- lethality[c(2,4),])
(lethality$type <- c(paste(vessels[3:4], collapse=' | '),
                     paste(vessels[1:2], collapse=' | ')))
lethality

# Run outcome predictions ======================================================

predict_outcomes(traffic,
                 whale,
                 seasonal,
                 p_encounter_dir,
                 surface,
                 avoidance,
                 lethality,
                 outcome_dir,
                 iterations = 1000)


################################################################################
################################################################################
# LNG Canada - worst case (flat 12 kn always)

# Prep datasets ================================================================

# Traffic
traffic <- readRDS('tests/vessels_lng_canada_12knots.RData')

# Encounter rate
p_encounter_dir <- 'tests/fw/impacts_lng_canada/12_knots/p_encounter/'

# Output directory
outcome_dir <- 'tests/fw/impacts_lng_canada/12_knots/outcomes/'

# Avoidance
avoidance <- readRDS('tests/p_avoidance.RData')
(avoidance <- avoidance[c(2,4),])
(avoidance$type <- c(paste(vessels[3:4], collapse=' | '),
                     paste(vessels[1:2], collapse=' | ')))
avoidance

# Lethality
lethality <- readRDS('tests/p_lethal.RData')
(lethality <- lethality[c(2,4),])
(lethality$type <- c(paste(vessels[3:4], collapse=' | '),
                     paste(vessels[1:2], collapse=' | ')))
lethality

# Run outcome predictions ======================================================

predict_outcomes(traffic,
                 whale,
                 seasonal,
                 p_encounter_dir,
                 surface,
                 avoidance,
                 lethality,
                 outcome_dir,
                 iterations = 1000)


################################################################################
################################################################################
# LNG Canada - probable case (8 - 14 knots)

# Prep datasets ================================================================

# Traffic
traffic <- readRDS('tests/vessels_lng_canada_8_14knots.RData')

# Encounter rate
p_encounter_dir <- 'tests/fw/impacts_lng_canada/8_14_knots/p_encounter/'

# Output directory
outcome_dir <- 'tests/fw/impacts_lng_canada/8_14_knots/outcomes/'

# Avoidance
avoidance <- readRDS('tests/p_avoidance.RData')
(avoidance <- avoidance[c(2,4),])
(avoidance$type <- c(paste(vessels[3:4], collapse=' | '),
                     paste(vessels[1:2], collapse=' | ')))
avoidance

# Lethality
lethality <- readRDS('tests/p_lethal.RData')
(lethality <- lethality[c(2,4),])
(lethality$type <- c(paste(vessels[3:4], collapse=' | '),
                     paste(vessels[1:2], collapse=' | ')))
lethality

# Run outcome predictions ======================================================

predict_outcomes(traffic,
                 whale,
                 seasonal,
                 p_encounter_dir,
                 surface,
                 avoidance,
                 lethality,
                 outcome_dir,
                 iterations = 1000)


