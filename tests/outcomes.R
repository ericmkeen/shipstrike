################################################################################
# outcomes
################################################################################

n=10 # say there were n grid cell transits within this type/month/diel

outcome(p_whale = runif(n, 0.001, .99),
        p_encounter = runif(n, 0.001, .99),
        p_surface = runif(n, 0.001, .99),
        p_avoidance = runif(n, 0.001, .99),
        p_lethality = runif(n, 0.001, .99)) %>%
  apply(2,sum)

################################################################################
