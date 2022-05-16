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

# LNG Canada only (probable case: variable speed, 8 - 14 knots) ) ==============
outcome_dir <- c('tests/fw/impacts_lng_canada/8_14_knots/outcomes/')
MR <- compile_results(outcome_dir)

# LNG Canada only (best case: knots as proposed) ) =============================
outcome_dir <- c('tests/fw/impacts_lng_canada/proposed_speed/outcomes/')
MR <- compile_results(outcome_dir)

# LNG Canada only (worst case: constant 12 knots) ) ============================
#outcome_dir <- c('tests/fw/impacts_lng_canada/12_knots/outcomes/')
#MR <- compile_results(outcome_dir)

# 2030 (2019 + LNG Canada probable case) =======================================
outcome_dir <- c('tests/fw/impacts_2019/outcomes/',
                 'tests/fw/impacts_lng_canada/8_14_knots/outcomes/')
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

