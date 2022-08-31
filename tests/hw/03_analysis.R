################################################################################
################################################################################
# Humpback whales ANALYSIS
################################################################################
################################################################################

library(dplyr)
library(ggplot2)
library(devtools)
document()

################################################################################
################################################################################
# Encounter rate

# Common inputs
(whales <- humpback_params())
runs <- 100
iterations <- 100

# Vessels 2019
# data(ais_2019) ; vessels <- ais_2019
# outcome_dir <- 'tests/hw/ais_2019/'
# encounter_rate(vessels, whales, outcome_dir,
#                month_batches = list(winter = c(0:4, 11:12),
#                                     summer = 5:10),
#                runs  = runs, iterations = iterations, toplot = FALSE)

# Vessels 2015
# data(ais_2015)
# encounter_rate(vessels = ais_2015,
#                whales = whales,
#                outcome_dir = 'tests/hw/ais_2015/',
#                month_batches = list(winter = c(0:4, 11:12),
#                                     summer = 5:10),
#                runs  = 100, iterations = 100, toplot = FALSE)

# LNG Canada
# data(lng_canada) ; vessels <- lng_canada
# outcome_dir <- 'tests/hw/lng_canada/'
# encounter_rate(vessels, whales, outcome_dir,
#                month_batches = list(all = 1:12),
#                runs  = runs, iterations = iterations, toplot = TRUE)

# Cedar LNG
# data(cedar_lng) ; vessels <- cedar_lng
# outcome_dir <- 'tests/hw/cedar_lng/'
# encounter_rate(vessels, whales, outcome_dir,
#                month_batches = list(all = 1:12),
#                runs  = runs, iterations = iterations, toplot = FALSE)


################################################################################
################################################################################
# Predict outcomes

# Common datasets to all analyses ==============================================

species <- 'hw'
whale <- readRDS('tests/hw/pwhale_seasonal_boots.RData')
seasonal <- NULL

data(p_surface)
surface <- p_surface

asymptote_scaling <- NULL

# 2019 traffic  ================================================================

data(ais_2019) ; traffic <- ais_2019
(vessels <- unique(traffic$type))

p_encounter_dir <- NULL
(outcome_dir <- 'tests/hw/ais_2019/') %>% dir

data(p_collision) ; (avoidance <- p_collision)
data(p_lethality) ; (lethality <- p_lethality)

month_batches <- list(winter = c(0:4, 11:12), summer = 5:10)

outcome_predict(traffic,
                scale_factors=NULL,
                 whale,
                 seasonal,
                 p_encounter_dir,
                 surface,
                 avoidance,
                 lethality,
                 outcome_dir,
                 asymptote_scaling = asymptote_scaling,
                 month_batches = month_batches,
                 species=species,
                 year=2019,
                 iterations = 1000)


# 2015 traffic  ================================================================

data(ais_2015)
data(p_collision)
data(p_lethality)
outcome_predict(traffic = ais_2015,
                scale_factors=NULL,
                whale = whale,
                seasonal = NULL,
                p_encounter_dir = NULL,
                surface = p_surface,
                avoidance = p_collision,
                lethality = p_lethality,
                outcome_dir = 'tests/hw/ais_2015/',
                asymptote_scaling = NULL,
                month_batches = list(winter = c(0:4, 11:12), summer = 5:10),
                species = species,
                year = 2015,
                iterations = 1000)

# 2030 traffic =================================================================

data(ais_2019) ; traffic <- ais_2019
(vessels <- unique(traffic$type))
(scale_factors <- readRDS('tests/ais/vessel_trends.RData'))

p_encounter_dir <- 'tests/hw/ais_2019/'
outcome_dir <- 'tests/hw/ais_2030/'

data(p_collision) ; (avoidance <- p_collision)
data(p_lethality) ; (lethality <- p_lethality)

month_batches <- list(winter = c(0:4, 11:12), summer = 5:10)

# Run outcome predictions
outcome_predict(traffic,
                scale_factors,
                 whale,
                 seasonal,
                 p_encounter_dir,
                 surface,
                 avoidance,
                 lethality,
                 outcome_dir,
                 asymptote_scaling = asymptote_scaling,
                 month_batches = month_batches,
                 species=species,
                 year=2030,
                 iterations = 1000)


# LNG Canada (8 - 14 knots)  ===================================================

data(lng_canada) ; traffic <- lng_canada
(vessels <- unique(traffic$type))

p_encounter_dir <- NULL
outcome_dir <- 'tests/hw/lng_canada/'

data(p_collision) ; (avoidance <- p_collision)
data(p_lethality) ; (lethality <- p_lethality)

(avoidance <- avoidance[c(2,4),])
(avoidance$type <- c(paste(vessels[3:4], collapse=' | '),
                     paste(vessels[1:2], collapse=' | ')))
avoidance

(lethality <- lethality[c(2,4),])
(lethality$type <- c(paste(vessels[3:4], collapse=' | '),
                     paste(vessels[1:2], collapse=' | ')))
lethality

month_batches <- list(all = 0:12)

# Run outcome predictions
outcome_predict(traffic,
                scale_factors=NULL,
                 whale,
                 seasonal,
                 p_encounter_dir,
                 surface,
                 avoidance,
                 lethality,
                 outcome_dir,
                 asymptote_scaling = asymptote_scaling,
                 month_batches = month_batches,
                 species=species,
                 year=2030,
                 iterations = 1000)


# Cedar LNG (8 - 14 knots)  ====================================================

data(cedar_lng) ; traffic <- cedar_lng
(vessels <- unique(traffic$type))

p_encounter_dir <- NULL
outcome_dir <- 'tests/hw/cedar_lng/'

data(p_collision) ; (avoidance <- p_collision)
data(p_lethality) ; (lethality <- p_lethality)

(avoidance <- avoidance[c(2,4),])
(avoidance$type <- c(paste(vessels[3:4], collapse=' | '),
                     paste(vessels[1:2], collapse=' | ')))
avoidance

(lethality <- lethality[c(2,4),])
(lethality$type <- c(paste(vessels[3:4], collapse=' | '),
                     paste(vessels[1:2], collapse=' | ')))
lethality

month_batches <- list(all = 0:12)

# Run outcome predictions
outcome_predict(traffic,
                scale_factors=NULL,
                whale,
                seasonal,
                p_encounter_dir,
                surface,
                avoidance,
                lethality,
                outcome_dir,
                asymptote_scaling = asymptote_scaling,
                month_batches = month_batches,
                species=species,
                year=2030,
                iterations = 1000)

################################################################################
################################################################################
# Put all outcomes in a list

hw <- list(params = humpback_params(),
           p_encounter = list(ais_2015 = readRDS('tests/hw/ais_2015/p_encounter.RData'),
                              ais_2019 = readRDS('tests/hw/ais_2019/p_encounter.RData'),
                              lng_canada = readRDS('tests/hw/lng_canada/p_encounter.RData'),
                              cedar_lng = readRDS('tests/hw/cedar_lng/p_encounter.RData')),
           outcomes = list(ais_2015 = readRDS('tests/hw/ais_2015/outcomes.RData'),
                           ais_2019 = readRDS('tests/hw/ais_2019/outcomes.RData'),
                           ais_2030 = readRDS('tests/hw/ais_2030/outcomes.RData'),
                           lng_canada = readRDS('tests/hw/lng_canada/outcomes.RData'),
                           cedar_lng = readRDS('tests/hw/cedar_lng/outcomes.RData')),
           grid = list(ais_2015 = readRDS('tests/hw/ais_2015/outcomes_grid.RData'),
                       ais_2019 = readRDS('tests/hw/ais_2019/outcomes_grid.RData'),
                       ais_2030 = readRDS('tests/hw/ais_2030/outcomes_grid.RData'),
                       lng_canada = readRDS('tests/hw/lng_canada/outcomes_grid.RData'),
                       cedar_lng = readRDS('tests/hw/cedar_lng/outcomes_grid.RData')))

# Review
hw$p_encounter$ais_2015 %>% head

hw$outcomes$ais_2015 %>% head(1)
hw$outcomes$ais_2019 %>% head(1)
hw$outcomes$ais_2030 %>% head(1)

hw$grid$ais_2015 %>% head(1)
hw$grid$ais_2019 %>% head(1)
hw$grid$ais_2030 %>% head(1)

# Make any fixes needed here

# Save to file
saveRDS(hw, file='tests/hw/results.RData')
