################################################################################
# Results - fin whales
################################################################################
# Libraries & package datasets

library(dplyr)
library(ggplot2)
library(bangarang)
library(devtools)
library(truncnorm)
library(ggpubr)
load_all()

################################################################################
# Combine results

outcome_dir <- 'tests/fw/impacts_lng_canada/12_knots/outcomes/'
lf <- list.files(outcome_dir)
(lf <- paste0(outcome_dir,lf))

mr <- data.frame()
for(i in 1:length(lf)){
  (lfi <- lf[i])
  message(lfi)
  mri <- readRDS(lfi)
  mr <- rbind(mr, mri)
}

nrow(mr)


mr$channel[mr$channel == 'CAA'] <- 'Caamano'
mr$channel[mr$channel == 'EST'] <- 'Estevan'
mr$channel[mr$channel == 'CMP'] <- 'Campania'
mr$channel[mr$channel == 'SQU'] <- 'Squally'
mr$channel[mr$channel == 'WHA'] <- 'Whale'
mr$channel[mr$channel == 'WRI'] <- 'Wright'
mr$channel[mr$channel == 'MCK'] <- 'McKay'
mr$channel[mr$channel == 'VER'] <- 'Verney'
mr$channel <- factor(mr$channel, levels=c('Caamano','Estevan','Campania','Squally','Whale','Wright','McKay','Verney'))

################################################################################

head(mr)

mrs <-
  mr %>%
  group_by(iteration) %>%
  summarize(across(cooccurrence:mortality2.3, sum)) %>%
  rename(collision = collision2.2,
         mortality = mortality2.2) %>%
  select(-iteration)

head(mrs)

outcome_histograms(mrs)

outcome_table(mrs)

length(which(mrs$mortality == 0)) / nrow(mrs)
length(which(mrs$mortality > 0)) / nrow(mrs)
length(which(mrs$mortality == 1)) / nrow(mrs)
length(which(mrs$mortality == 2)) / nrow(mrs)

# outcome_rates
mr %>%
  group_by(vessel, iteration) %>%
  summarize(across(cooccurrence:mortality2.3, sum)) %>%
  rename(collision = collision2.2,
         mortality = mortality2.2) %>%
  select(-iteration) %>%
  transmute(penc = encounter / cooccurrence,
            psur = surface / encounter,
            pcol = collision / surface,
            pmor = mortality / collision) %>%
  summarize(penc_mean = mean(penc, na.rm=TRUE),
            psur_mean = mean(psur, na.rm=TRUE),
            pcol_mean = mean(pcol, na.rm=TRUE),
            pmor_mean = mean(pmor, na.rm=TRUE))


################################################################################

dfi <-
  mr %>%
  group_by(month, iteration) %>%
  summarize(across(cooccurrence:mortality2.3, sum)) %>%
  rename(collision = collision2.2,
         mortality = mortality2.2)

head(dfi)

dfe <- data.frame()
events <- c('cooccurrence','encounter','surface','collision','mortality')
for(i in 1:length(events)){
  (eventi <- events[i])
  (keeper <- which(names(dfi)==eventi))
  (evi <- dfi[,keeper] %>% as.data.frame)
  evi <- evi[,1] %>% as.numeric
  dfevi <- data.frame(month = dfi$month, iteration = dfi$iteration, eventi = i, event = eventi, outcome = evi)
  dfe <- rbind(dfe, dfevi)
}

dfe$event <- factor(dfe$event, levels = events)

ggplot(dfe, aes(x=outcome, y=factor(month))) +
  geom_violin() +
  xlab('Number of interactions') +
  ylab('Month') +
  facet_wrap(~event, scales='free_x')


################################################################################

dfi <-
  mr %>%
  group_by(diel, iteration) %>%
  summarize(across(cooccurrence:mortality2.3, sum)) %>%
  rename(collision = collision2.2,
         mortality = mortality2.2)
head(dfi)

dfe <- data.frame()
events <- c('cooccurrence','encounter','surface','collision','mortality')
for(i in 1:length(events)){
  (eventi <- events[i])
  (keeper <- which(names(dfi)==eventi))
  (evi <- dfi[,keeper] %>% as.data.frame)
  evi <- evi[,1] %>% as.numeric
  dfevi <- data.frame(diel = dfi$diel, iteration = dfi$iteration, eventi = i, event = eventi, outcome = evi)
  dfe <- rbind(dfe, dfevi)
}

dfe$event <- factor(dfe$event, levels = events)

ggplot(dfe, aes(x=diel, y=outcome)) +
  geom_violin(adjust=2) +
  ylab('Number of interactions') +
  xlab('Diel period') +
  facet_wrap(~event, scales='free_y')


################################################################################
# By vessel class

dfi <-
  mr %>%
  group_by(vessel, iteration) %>%
  summarize(across(cooccurrence:mortality2.3, sum)) %>%
  rename(collision = collision2.2,
         mortality = mortality2.2)

head(dfi)

dfe <- data.frame()
events <- c('cooccurrence','encounter','surface','collision','mortality')
for(i in 1:length(events)){
  (eventi <- events[i])
  (keeper <- which(names(dfi)==eventi))
  (evi <- dfi[,keeper] %>% as.data.frame)
  evi <- evi[,1] %>% as.numeric
  dfevi <- data.frame(vessel = dfi$vessel, iteration = dfi$iteration, eventi = i, event = eventi, outcome = evi)
  dfe <- rbind(dfe, dfevi)
}

dfe$event <- factor(dfe$event, levels = events)

ggplot(dfe, aes(x=outcome, y=vessel)) +
  geom_violin(width=1.5) +
  xlab('Number of Interactions') +
  ylab(NULL) +
  facet_wrap(~event, scales='free_x')


tot_mort <- sum(dfi$mortality)

dfii <-
  dfi %>%
  group_by(vessel) %>%
  summarize(across(cooccurrence:mortality, sum))
dfii

(tots <- apply(dfii[2:ncol(dfii)], 2, sum))

i=2
for(i in 2:ncol(dfii)){
  dfii[,i] <- round((dfii[,i] / tots[i-1]),4)*100
}

dfii %>% as.data.frame


################################################################################
# By channel

dfi <-
  mr %>%
  group_by(channel, iteration) %>%
  summarize(across(cooccurrence:mortality2.3, sum)) %>%
  rename(collision = collision2.2,
         mortality = mortality2.2)

head(dfi)


dfe <- data.frame()
events <- c('cooccurrence','encounter','surface','collision','mortality')
for(i in 1:length(events)){
  (eventi <- events[i])
  (keeper <- which(names(dfi)==eventi))
  (evi <- dfi[,keeper] %>% as.data.frame)
  evi <- evi[,1] %>% as.numeric
  dfevi <- data.frame(channel = dfi$channel, iteration = dfi$iteration, eventi = i, event = eventi, outcome = evi)
  dfe <- rbind(dfe, dfevi)
}

dfe$event <- factor(dfe$event, levels = events)

ggplot(dfe, aes(x=outcome, y=channel)) +
  geom_violin() +
  xlab('Number of interactions') +
  ylab(NULL) +
  facet_wrap(~event, scales='free_x')


mr %>% head

################################################################################




