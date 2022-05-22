################################################################################
# Combined impacts analysis -- FIN WHALES
################################################################################
################################################################################

library(dplyr)
library(ggplot2)
library(devtools)
library(knitr)
document()

################################################################################
################################################################################
# Compile results
################################################################################
# FW
################################################################################

# 2019 only ====================================================================
outcome_dir <- c('tests/fw/impacts_2019/outcomes/')
MR <- compile_results(outcome_dir)

# 2030 only ====================================================================
outcome_dir <- c('tests/fw/impacts_2030/outcomes/')
MR <- compile_results(outcome_dir)

# LNG Canada only (probable case: variable speed, 8 - 14 knots) ) ==============
outcome_dir <- c('tests/fw/impacts_lng_canada/8_14_knots/outcomes/')
MR <- compile_results(outcome_dir)

# Cedar LNG only (probable case: variable speed, 8 - 14 knots) ) ==============
outcome_dir <- c('tests/fw/impacts_cedar_lng/8_14_knots/outcomes/')
MR <- compile_results(outcome_dir)

# 2030 (2019 + LNG Canada probable case) =======================================
outcome_dir <- c('tests/fw/impacts_2030/outcomes/',
                 'tests/fw/impacts_lng_canada/8_14_knots/outcomes/',
                 'tests/fw/impacts_cedar_lng/8_14_knots/outcomes/')
MR <- compile_results(outcome_dir)


################################################################################
# Explore results

MR %>% names

# Tables
MR$grand_table %>% kable
MR$chances %>% kable
MR$share_vessels
MR$share_channels
MR$share_months
MR$share_diel
MR$projections %>% kable

# Plots
MR$gg_outcomes
MR$gg_outcomes_all

MR$ggraw_monthly
MR$ggraw_vessel
MR$ggraw_channel
MR$ggraw_diel

MR$ggshare_vessels
MR$ggshare_channels
MR$ggshare_months
MR$ggshare_diel

MR$ggproj_cooccurrence
MR$ggproj_encounter
MR$ggproj_surface
MR$ggproj_collision
MR$ggproj_mortality

################################################################################

