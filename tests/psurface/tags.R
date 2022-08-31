################################################################################
# DFO fin whale tag data analysis
################################################################################

library(rstudioapi)
library(dplyr)
library(ggplot2)
library(lubridate)
library(stringr)
library(readr)
library(oce)

df <- readRDS('tests/psurface/tag_data.RData')

# See supp mat for graphs tables etc

# remove id 7
df <- df %>% dplyr::filter(id != 7)

################################################################################
# For each depth, get proportion of records shallower than it

mr <- data.frame()
i=1
for(i in 1:length(unique(df$id))){
  idi <- i
  dfi <- df %>% dplyr::filter(id == idi)

  zs <- seq(0,250,by=0.5)
  zi <- 10

  dieli <- c('night','day')
  for(di in dieli){
    dfid <- dfi %>% dplyr::filter(diel == di)
    props <- c()
    for(zi in zs){
      (n_shallower <- length(which(dfid$z <= zi)))
      (propi <- n_shallower / nrow(dfid))
      props <- c(props, propi)
    }
    mri <- data.frame(id=i, z = zs, diel = di, prop = props)
    mr <- rbind(mr, mri)
  }

}

nrow(mr)

################################################################################
# Summarize these depth proportions

mr_summ <-
  mr %>%
  group_by(diel, z) %>%
  summarize(p_mean = mean(prop),
            p_sd = sd(prop))

mr_summ %>% filter(z %in% c(1,2,5,10,15,20,25,30))

################################################################################
# Save results

getwd()
p_surface <- mr_summ
saveRDS(p_surface, file='data-raw/p_surface.RData')

################################################################################
