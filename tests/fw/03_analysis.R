################################################################################
################################################################################
# Fin whales ANALYSIS
################################################################################
################################################################################

library(dplyr)
library(ggplot2)
library(devtools)
document()

################################################################################
################################################################################
# Encounter rate

# Try it out and see details ===================================================

if(FALSE){
data(lng_canada)
mr <- encounter_rate(vessels = lng_canada,
                     whales = fin_params(),
                     outcome_dir = 'tests/',
                     month_batches = list(all = 1:12),
                     runs  = 1, iterations = 100, toplot = TRUE)

# Check results as recorded on file
penc <- readRDS('tests/p_encounter.RData')
penc

# Review detailed output
nrow(mr)
head(mr)
mr$encounter %>% table(useNA='ifany')
mr$type %>% unique

# Filter to single type and diel period
(mri <- mr %>% filter(type=='LNG Canada tanker in-heel',
                      diel=='day')) %>% nrow

# Try out diagnostics
ggenc <- encounter_diagnostics(mri)

#ggenc <- encounter_transit(mri, id=1:3, wait = FALSE)
#ggenc[[2]]
}


# Common inputs ================================================================
(whales <- fin_params())
runs <- 100
iterations <- 100

# Vessels 2019
# data(ais_2019) ; vessels <- ais_2019
# outcome_dir <- 'tests/fw/ais_2019/'
# encounter_rate(vessels, whales, outcome_dir,
#                month_batches = list(winter = c(0:4, 11:12),
#                                     summer = 5:10),
#                runs  = runs, iterations = iterations, toplot = FALSE)

# Vessels 2015
# data(ais_2015)
# encounter_rate(vessels = ais_2015,
#                whales = whales,
#                outcome_dir = 'tests/fw/ais_2015/',
#                month_batches = list(winter = c(0:4, 11:12),
#                                     summer = 5:10),
#                runs  = 100, iterations = 100, toplot = FALSE)

# LNG Canada
# data(lng_canada) ; vessels <- lng_canada
# outcome_dir <- 'tests/fw/lng_canada/'
# encounter_rate(vessels, whales, outcome_dir,
#                month_batches = list(all = 1:12),
#                runs  = runs, iterations = iterations, toplot = TRUE)

# Cedar LNG
# data(cedar_lng) ; vessels <- cedar_lng
# outcome_dir <- 'tests/fw/cedar_lng/'
# encounter_rate(vessels, whales, outcome_dir,
#                month_batches = list(all = 1:12),
#                runs  = runs, iterations = iterations, toplot = FALSE)


################################################################################
################################################################################
# Predict outcomes

# Common datasets to all analyses ==============================================

species <- 'fw'
load('tests/fw/dsm-bootstraps.RData')
whale <- bootstraps

load('tests/fw/seasonal_posterior.RData')
seasonal <- seasonal_boot

data(p_surface)
surface <- p_surface

asymptote_scaling <- NULL

# 2019 traffic  ================================================================

# data(ais_2019) ; traffic <- ais_2019
# (vessels <- unique(traffic$type))
#
# p_encounter_dir <- NULL
# outcome_dir <- 'tests/fw/ais_2019/'
#
# data(p_collision) ; (avoidance <- p_collision)
# data(p_lethality) ; (lethality <- p_lethality)
#
# month_batches <- list(winter = c(0:4, 11:12), summer = 5:10)
#
# outcome_predict(traffic,
#                 scale_factors=NULL,
#                 whale,
#                 seasonal,
#                 p_encounter_dir,
#                 surface,
#                 avoidance,
#                 lethality,
#                 outcome_dir,
#                 asymptote_scaling = asymptote_scaling,
#                 month_batches = month_batches,
#                 species = species,
#                 year = 2019,
#                 iterations = 1000)


# 2015 traffic  ================================================================

# data(ais_2015)
# data(p_collision)
# data(p_lethality)
# outcome_predict(traffic = ais_2015,
#                 scale_factors=NULL,
#                 whale = whale,
#                 seasonal = seasonal,
#                 p_encounter_dir = NULL,
#                 surface = p_surface,
#                 avoidance = p_collision,
#                 lethality = p_lethality,
#                 outcome_dir = 'tests/fw/ais_2015/',
#                 asymptote_scaling = NULL,
#                 month_batches = list(winter = c(0:4, 11:12), summer = 5:10),
#                 species = species,
#                 year = 2015,
#                 iterations = 1000)

# 2030 traffic =================================================================

# data(ais_2019) ; traffic <- ais_2019
# (vessels <- unique(traffic$type))
# (scale_factors <- readRDS('tests/ais/vessel_trends.RData'))
#
# p_encounter_dir <- 'tests/fw/ais_2019/'
# outcome_dir <- 'tests/fw/ais_2030/'
#
# data(p_collision) ; (avoidance <- p_collision)
# data(p_lethality) ; (lethality <- p_lethality)
#
# month_batches <- list(winter = c(0:4, 11:12), summer = 5:10)
#
# # Run outcome predictions
# outcome_predict(traffic,
#                 scale_factors,
#                 whale,
#                 seasonal,
#                 p_encounter_dir,
#                 surface,
#                 avoidance,
#                 lethality,
#                 outcome_dir,
#                 asymptote_scaling = asymptote_scaling,
#                 month_batches = month_batches,
#                 species = species,
#                 year = 2030,
#                 iterations = 1000)


# LNG Canada (8 - 14 knots)  ===================================================

data(lng_canada) ; traffic <- lng_canada
(vessels <- unique(traffic$type))

p_encounter_dir <- NULL
outcome_dir <- 'tests/fw/lng_canada/'

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
                species = species,
                year = 2030,
                iterations = 1000)


# Cedar LNG (8 - 14 knots)  ====================================================

data(cedar_lng) ; traffic <- cedar_lng
(vessels <- unique(traffic$type))

p_encounter_dir <- NULL
outcome_dir <- 'tests/fw/cedar_lng/'

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
                species = species,
                year = 2030,
                iterations = 1000)


################################################################################
################################################################################
# Put all outcomes in a list

fw <- list(params = fin_params(),
           p_encounter = list(ais_2015 = readRDS('tests/fw/ais_2015/p_encounter.RData'),
                              ais_2019 = readRDS('tests/fw/ais_2019/p_encounter.RData'),
                              lng_canada = readRDS('tests/fw/lng_canada/p_encounter.RData'),
                              cedar_lng = readRDS('tests/fw/cedar_lng/p_encounter.RData')),
           outcomes = list(ais_2015 = readRDS('tests/fw/ais_2015/outcomes.RData'),
                           ais_2019 = readRDS('tests/fw/ais_2019/outcomes.RData'),
                           ais_2030 = readRDS('tests/fw/ais_2030/outcomes.RData'),
                           lng_canada = readRDS('tests/fw/lng_canada/outcomes.RData'),
                           cedar_lng = readRDS('tests/fw/cedar_lng/outcomes.RData')),
           grid = list(ais_2015 = readRDS('tests/fw/ais_2015/outcomes_grid.RData'),
                       ais_2019 = readRDS('tests/fw/ais_2019/outcomes_grid.RData'),
                       ais_2030 = readRDS('tests/fw/ais_2030/outcomes_grid.RData'),
                       lng_canada = readRDS('tests/fw/lng_canada/outcomes_grid.RData'),
                       cedar_lng = readRDS('tests/fw/cedar_lng/outcomes_grid.RData')))

# Review
fw$p_encounter$ais_2015 %>% head

fw$outcomes$ais_2015 %>% head(1)
fw$outcomes$ais_2019 %>% head(1)
fw$outcomes$ais_2030 %>% head(1)

fw$grid$ais_2015 %>% head(1)
fw$grid$ais_2019 %>% head(1)
fw$grid$ais_2030 %>% head(1)

fw$outcomes$cedar_lng %>% outcome_table %>% head

# Make any fixes needed here

# Save as a RData object
saveRDS(fw, file='tests/fw/results.RData')



