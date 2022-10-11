################################################################################
# Mitigation scenarios -- FIN WHALES
################################################################################
################################################################################

library(dplyr)
library(ggplot2)
library(devtools)
document()

################################################################################
################################################################################
################################################################################
################################################################################
# (1) Speed reductions

################################################################################
# 1a	LNG-specific speed reduction (7 – 9 kn)

# LNG Canada ===================================================================
#
# # Encounter rate
# data(lng_canada)
# encounter_rate(vessels = lng_canada,
#                whales = fin_params(),
#                outcome_dir = 'tests/fw/mitigation/1a/lng_canada/',
#                month_batches = list(all = 1:12),
#                new_speeds = runif(100, 7, 9),
#                runs  = 100, iterations = 100, toplot = TRUE)
#
# # # Outcomes
# load('tests/fw/dsm-bootstraps.RData')
# load('tests/fw/seasonal_posterior.RData')
# data(lng_canada)
# (vessels <- unique(lng_canada$type))
# data(p_surface)
#
# data(p_collision) ; (avoidance <- p_collision)
# data(p_lethality) ; (lethality <- p_lethality)
# (avoidance <- avoidance[c(2,4),])
# (avoidance$type <- c(paste(vessels[3:4], collapse=' | '),
#                      paste(vessels[1:2], collapse=' | ')))
# (lethality <- lethality[c(2,4),])
# (lethality$type <- c(paste(vessels[3:4], collapse=' | '),
#                      paste(vessels[1:2], collapse=' | ')))
#
# lng_canada_mod <- lng_canada
# lng_canada_mod$speed <- runif(nrow(lng_canada_mod), 7, 9)
#
# outcome_predict(traffic = lng_canada_mod,
#                 scale_factors=NULL,
#                 whale = bootstraps,
#                 seasonal = seasonal_boot,
#                 p_encounter_dir = NULL,
#                 surface = p_surface,
#                 avoidance = avoidance,
#                 lethality = lethality,
#                 outcome_dir = 'tests/fw/mitigation/1a/lng_canada/',
#                 asymptote_scaling = NULL,
#                 month_batches = list(all = 0:12),
#                 iterations = 1000)

# # Cedar LNG ====================================================================
#
# # Encounter rate
# data(cedar_lng)
# encounter_rate(vessels = cedar_lng,
#                whales = fin_params(),
#                outcome_dir = 'tests/fw/mitigation/1a/cedar_lng/',
#                month_batches = list(all = 1:12),
#                new_speeds = runif(100, 7, 9),
#                runs  = 100, iterations = 100, toplot = TRUE)
#
# # Outcomes
# load('tests/fw/dsm-bootstraps.RData')
# load('tests/fw/seasonal_posterior.RData')
# data(cedar_lng)
# (vessels <- unique(cedar_lng$type))
# data(p_surface)
# data(p_collision) ; (avoidance <- p_collision)
# data(p_lethality) ; (lethality <- p_lethality)
# (avoidance <- avoidance[c(2,4),])
# (avoidance$type <- c(paste(vessels[3:4], collapse=' | '),
#                      paste(vessels[1:2], collapse=' | ')))
# (lethality <- lethality[c(2,4),])
# (lethality$type <- c(paste(vessels[3:4], collapse=' | '),
#                      paste(vessels[1:2], collapse=' | ')))
#
# cedar_lng_mod <- cedar_lng
# cedar_lng_mod$speed <- runif(nrow(cedar_lng_mod), 7, 9)
#
# outcome_predict(traffic = cedar_lng_mod,
#                 scale_factors=NULL,
#                 whale = bootstraps,
#                 seasonal = seasonal_boot,
#                 p_encounter_dir = NULL,
#                 surface = p_surface,
#                 avoidance = avoidance,
#                 lethality = lethality,
#                 outcome_dir = 'tests/fw/mitigation/1a/cedar_lng/',
#                 asymptote_scaling = NULL,
#                 month_batches = list(all = 0:12),
#                 iterations = 1000)

################################################################################
# 1b	Speed reduction for all traffic > 100m (7 – 9 kn for LNG, < 10 kn for all other)

# Encounter rate: AIS 2030
# data(ais_2019)
# encounter_rate(vessels = ais_2019,
#                whales = fin_params(),
#                outcome_dir = 'tests/fw/mitigation/1b/ais_2030/',
#                month_batches = list(all = 1:12),
#                speed_restriction = 9,
#                lengths_restricted = 180,
#                runs  = 100, iterations = 100, toplot = FALSE)

# Predict outcomes on AIS 2030 with < 9 kn
load('tests/fw/dsm-bootstraps.RData')
load('tests/fw/seasonal_posterior.RData')
data(p_surface)
data(ais_2019)
scale_factors <- readRDS('tests/ais/vessel_trends.RData')
data(p_collision)
data(p_lethality)
#
# # Modify AIS data
ais_2019_mod <- ais_2019
bads <- which(ais_2019_mod$length > 180 & ais_2019_mod$speed > 9)
ais_2019_mod$speed[bads] <- 9

outcome_predict(traffic = ais_2019_mod,
                scale_factors = scale_factors,
                whale = bootstraps,
                seasonal = seasonal_boot,
                p_encounter_dir = 'tests/fw/mitigation/1b/ais_2030/',
                surface = p_surface,
                avoidance = p_collision,
                lethality = p_lethality,
                outcome_dir = 'tests/fw/mitigation/1b/ais_2030/',
                asymptote_scaling = NULL,
                month_batches = list(winter = c(0:4, 11:12), summer = 5:10),
                species='fw',
                year=2030,
                iterations = 1000)

################################################################################
################################################################################
################################################################################
################################################################################
# (2) Daytime-only transits

################################################################################
# 2a	LNG-only

# alter LNG traffic to day, then re-run predict outcomes
# do so for LNG Canada
# do so for Cedar LNG

# LNG Canada ===================================================================
#
# load('tests/fw/dsm-bootstraps.RData')
# load('tests/fw/seasonal_posterior.RData')
# data(p_surface)
# data(lng_canada)
# (vessels <- unique(lng_canada$type))
#
# lng_canada_mod <- lng_canada # Modify diel here
# lng_canada_mod$diel <- 'day'
#
# data(p_collision) ; (avoidance <- p_collision)
# data(p_lethality) ; (lethality <- p_lethality)
# (avoidance <- avoidance[c(2,4),])
# (avoidance$type <- c(paste(vessels[3:4], collapse=' | '),
#                      paste(vessels[1:2], collapse=' | ')))
# (lethality <- lethality[c(2,4),])
# (lethality$type <- c(paste(vessels[3:4], collapse=' | '),
#                      paste(vessels[1:2], collapse=' | ')))
#
# outcome_predict(traffic = lng_canada_mod,
#                 scale_factors=NULL,
#                 whale = bootstraps,
#                 seasonal = seasonal_boot,
#                 p_encounter_dir = 'tests/fw/lng_canada/',
#                 surface = p_surface,
#                 avoidance = avoidance,
#                 lethality = lethality,
#                 outcome_dir = 'tests/fw/mitigation/2a/lng_canada/',
#                 asymptote_scaling = NULL,
#                 month_batches = list(all = 0:12),
#                 iterations = 1000)

# Cedar LNG ====================================================================

# load('tests/fw/dsm-bootstraps.RData')
# load('tests/fw/seasonal_posterior.RData')
# data(p_surface)
# data(cedar_lng)
# (vessels <- unique(cedar_lng$type))
#
# cedar_lng_mod <- cedar_lng # Modify diel here
# cedar_lng_mod$diel <- 'day'
#
# data(p_collision) ; (avoidance <- p_collision)
# data(p_lethality) ; (lethality <- p_lethality)
# (avoidance <- avoidance[c(2,4),])
# (avoidance$type <- c(paste(vessels[3:4], collapse=' | '),
#                      paste(vessels[1:2], collapse=' | ')))
# (lethality <- lethality[c(2,4),])
# (lethality$type <- c(paste(vessels[3:4], collapse=' | '),
#                      paste(vessels[1:2], collapse=' | ')))
#
# outcome_predict(traffic = cedar_lng_mod,
#                 scale_factors=NULL,
#                 whale = bootstraps,
#                 seasonal = seasonal_boot,
#                 p_encounter_dir = 'tests/fw/cedar_lng/',
#                 surface = p_surface,
#                 avoidance = avoidance,
#                 lethality = lethality,
#                 outcome_dir = 'tests/fw/mitigation/2a/cedar_lng/',
#                 asymptote_scaling = NULL,
#                 month_batches = list(all = 0:12),
#                 iterations = 1000)

################################################################################
# 2b	All traffic > 100m)

# alter all traffic to day, then re-run predict outcomes
# do so for AIS 2030

load('tests/fw/dsm-bootstraps.RData')
load('tests/fw/seasonal_posterior.RData')
scale_factors <- readRDS('tests/ais/vessel_trends.RData')
data(p_collision)
data(p_lethality)
data(p_surface)
data(ais_2019)

ais_mod <- ais_2019 # Modify diel here
bads <- which(ais_mod$length > 180)
ais_mod$diel[bads] <- 'day'

outcome_predict(traffic = ais_mod,
                scale_factors=scale_factors,
                whale = bootstraps,
                seasonal = seasonal_boot,
                p_encounter_dir = 'tests/fw/ais_2019/',
                surface = p_surface,
                avoidance = p_collision,
                lethality = p_lethality,
                outcome_dir = 'tests/fw/mitigation/2b/ais_2030/',
                asymptote_scaling = NULL,
                month_batches = list(winter = c(0:4, 11:12), summer = 5:10),
                species = 'fw',
                year = 2030,
                iterations = 1000)


################################################################################
################################################################################
################################################################################
################################################################################
# (3) Seasonal displacement of LNG traffic

if(TRUE){

  # Baseline scenario (no mitigation)   ==========================================
  results <- readRDS('tests/fw/results.RData')
  outcomes <- rbind(results$outcomes$lng_canda,
                    results$outcomes$cedar_lng)

  # 3a	One month  ===============================================================
  mr <- mitigate_loop(outcomes,
                      mitigation_duration = 1,
                      reschedule = TRUE)
  mr
  saveRDS(mr, file='tests/fw/mitigation/3a/mitigation.RData')


  # 3b	Two months  ==============================================================
  mr <- mitigate_loop(outcomes,
                      mitigation_duration = 2,
                      reschedule = TRUE)
  saveRDS(mr, file='tests/fw/mitigation/3b/mitigation.RData')

  # 3c	Three months  ============================================================
  mr <- mitigate_loop(outcomes,
                      mitigation_duration = 3,
                      reschedule = TRUE)
  saveRDS(mr, file='tests/fw/mitigation/3c/mitigation.RData')

  ################################################################################
  ################################################################################
  ################################################################################
  ################################################################################
  # (4) Seasonal moratorium on LNG traffic

  # 4a	One month ================================================================
  mr <- mitigate_loop(outcomes,
                      mitigation_duration = 1,
                      reschedule = FALSE)
  saveRDS(mr, file='tests/fw/mitigation/4a/mitigation.RData')

  # 4b	Two months  ==============================================================
  mr <- mitigate_loop(outcomes,
                      mitigation_duration = 2,
                      reschedule = FALSE)
  saveRDS(mr, file='tests/fw/mitigation/4b/mitigation.RData')

  # 4c	Three months  ============================================================
  mr <- mitigate_loop(outcomes,
                      mitigation_duration = 3,
                      reschedule = FALSE)
  saveRDS(mr, file='tests/fw/mitigation/4c/mitigation.RData')

}

################################################################################
################################################################################
# Put all outcomes in a list

mitigations <- list(m1 = list(lng_canada = list(p_encounter = readRDS('tests/fw/mitigation/1a/lng_canada/p_encounter.RData'),
                                                outcomes = readRDS('tests/fw/mitigation/1a/lng_canada/outcomes.RData'),
                                                grid = readRDS('tests/fw/mitigation/1a/lng_canada/outcomes_grid.RData')),
                              cedar_lng = list(p_encounter = readRDS('tests/fw/mitigation/1a/cedar_lng/p_encounter.RData'),
                                               outcomes = readRDS('tests/fw/mitigation/1a/cedar_lng/outcomes.RData'),
                                               grid = readRDS('tests/fw/mitigation/1a/cedar_lng/outcomes_grid.RData')),
                              ais_2030 = list(p_encounter = readRDS('tests/fw/mitigation/1b/ais_2030/p_encounter.RData'),
                                              outcomes = readRDS('tests/fw/mitigation/1b/ais_2030/outcomes.RData'),
                                              grid = readRDS('tests/fw/mitigation/1b/ais_2030/outcomes_grid.RData'))),
                    m2 = list(lng_canada = list(outcomes = readRDS('tests/fw/mitigation/2a/lng_canada/outcomes.RData'),
                                                grid = readRDS('tests/fw/mitigation/2a/lng_canada/outcomes_grid.RData')),
                              cedar_lng = list(outcomes = readRDS('tests/fw/mitigation/2a/cedar_lng/outcomes.RData'),
                                               grid = readRDS('tests/fw/mitigation/2a/cedar_lng/outcomes_grid.RData')),
                              ais_2030 = list(outcomes = readRDS('tests/fw/mitigation/2b/ais_2030/outcomes.RData'),
                                              grid = readRDS('tests/fw/mitigation/2b/ais_2030/outcomes_grid.RData'))),
                    m3 = list(one_month = readRDS('tests/fw/mitigation/3a/mitigation.RData'),
                              two_months = readRDS('tests/fw/mitigation/3b/mitigation.RData'),
                              three_months = readRDS('tests/fw/mitigation/3c/mitigation.RData')),
                    m4 = list(one_month = readRDS('tests/fw/mitigation/4a/mitigation.RData'),
                              two_months = readRDS('tests/fw/mitigation/4b/mitigation.RData'),
                              three_months = readRDS('tests/fw/mitigation/4c/mitigation.RData')))

saveRDS(mitigations, file='tests/fw/mitigation/mitigation.RData')




