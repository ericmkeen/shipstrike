################################################################################
# outcomes
################################################################################

library(devtools)
load_all()

n=10 # say there were n grid cell transits within this type/month/diel

outcome(p_whale = runif(n, 0.001, .99),
        p_encounter = runif(n, 0.001, .99),
        p_surface = runif(n, 0.001, .99),
        p_avoidance = runif(n, 0.001, .99),
        p_lethality = runif(n, 0.001, .99)) %>%
  apply(2,sum)


################################################################################
# Load parameters

# vessels
load('tests/vessels_sim.RData')
vessels <- vessels_sim
vessels %>% head

# whale density
load('tests/fw/dsm-bootstraps.RData')
pwhale <- bootstraps
pwhale %>% head

# seasonal trend
load('tests/fw/seasonal_posterior.RData')
seasonal <- MR
seasonal %>% names

# encounter rate
load('tests/fw/p_encounter.RData')
pencounter <- results
pencounter %>% head

# surface

# avoidance

# lethality




################################################################################

geostrata <- c('KFS')
vessel_types <- c('tanker')
months <- 1:12
diels <- c('day','night')
gsi <- vi <- mi <- di <- 1
for(gsi in 1:length(geostrata)){
  (geostratum <- geostrata[gsi])
  for(vi in 1:length(vessel_types)){
    (vessi <- vessel_types[vi])
    for(mi in 1:length(months)){
      (monthi <- monthi[mi])
      for(di in 1:length(diels)){
        (dieli <- diels[di])
        #=======================================================================







        #=======================================================================
      } # end of diel period
    }# end of month
  } # end of vessel types
} # end of geostrata

################################################################################








################################################################################
