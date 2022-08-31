################################################################################
################################################################################
# AIS increase
################################################################################
################################################################################

library(readr)
library(dplyr)
library(bangarang)
library(devtools)
library(lubridate)
library(ggplot2)

################################################################################
################################################################################

data(ais_2014)
data(ais_2015)
data(ais_2018)
data(ais_2019)

ais <- rbind(ais_2014, ais_2015, ais_2018, ais_2019)

################################################################################
################################################################################
# Overall change

(aiskm <-
  ais %>%
  group_by(year) %>%
  summarize(km = sum(km)) %>%
    rename(yyyy = year))

(aislm <- lm(km ~ yyyy, data= aiskm)) %>% summary

(rati <- summary(aislm)$coefficients[2,1] / aiskm$km[aiskm$yyyy == 2019])

(aispred <- data.frame(yyyy=((2030:2039)-3)))
(aispred$km <- predict(aislm, newdata=aispred))
aispred$yyyy <- aispred$yyyy + 3
aispred

ggplot(aiskm, mapping=aes(x=year, y=km)) +
  geom_point() +
  geom_smooth(method = 'lm', se=FALSE) +
  ylim(0, 400000)


################################################################################
################################################################################
# AIS km

ais %>%
  group_by(type, year) %>%
  mutate(km = sum(km))

aisi <- data.frame(type = rep(unique(ais$type), each=length(unique(ais$year))),
                   year = rep(unique(ais$year), times=length(unique(ais$type))),
                   km = 0)
aisi
i=1
for(i in 1:nrow(aisi)){
  (aisii <- aisi[i,])
  aisty <- ais %>% filter(type == aisii$type, year == aisii$year)
  if(nrow(aisty)>0){
    aisi$km[i] <- sum(aisty$km)
  }
}
aisi

obs <- pred <- summ <- data.frame()
(types <- aisi$type %>% unique)
i=10
for(i in 1:length(types)){
  (typi <- types[i])
  (aiskm <- aisi %>%
      dplyr::filter(type == typi))

  (obsi <- aiskm) # data.frame(type = typi, aiskm) %>% rename(year = yyyy))
  obs <- rbind(obs, obsi)

  (aislm <- lm(km ~ year, data= aiskm)) %>% summary
  (pvali <- summary(aislm)$coefficients[2,4])

  (rati <- summary(aislm)$coefficients[2,1] / obsi$km[obsi$year == 2019])

  (aispred <- data.frame(year=((2029:2039)-3)))
  (aispred$km <- predict(aislm, newdata=aispred))
  aispred$year <- aispred$year + 3
  aispred
  aispred <-
    aispred %>%
    mutate(type = typi) %>%
    mutate(km_pre = dplyr::lag(km)) %>%
    mutate(diff = abs(km) - abs(km_pre)) %>%
    mutate(rate = diff / km) %>%
    select(type, year, km, rate) %>%
    filter(year == 2030)

    aispred
  (predi <- aispred)
  predi$km[predi$km < 0] <- 0
  pred <- rbind(pred, predi)

  summi <-
    aispred %>%
    group_by(type) %>%
    summarize(rate_2030 = mean(rate, na.rm=TRUE)) %>%
    mutate(rate_2019 = rati) %>%
    mutate(pvalue = pvali) %>%
    select(type, rate_2019, rate_2030, pvalue)

  summi
  summ <- rbind(summ, summi)

}

obs
pred
summ

saveRDS(obs, file='tests/ais/vessel_trends_obs.RData')

ggplot(obs, mapping=aes(x=year, y=km, color=type)) +
  geom_point(size=1.8) +
  geom_line() +
  ylab('Kilometers transited in study area') +
  xlab(NULL) +
  labs(color='Vessel class',
       title="Trends in Gitga'at area vessel traffic, 2014 - 2019")


obs %>% head
pred %>% head

(obs_join <- obs %>%
    group_by(type) %>%
    summarize(km_2014 = km[year == 2014],
              km_2015 = km[year == 2015],
              km_2018 = km[year == 2018],
              km_2019 = km[year == 2019]))

(pred_join <- pred %>% filter(year == 2030) %>% select(type, km_2030 = km))

ais_trends <- left_join(obs_join, pred_join, by='type')
(ais_trends$scale_factor <- ais_trends$km_2030 / ais_trends$km_2019)
ais_trends <- left_join(ais_trends, summ, by='type')

ais_trends
saveRDS(ais_trends, file='tests/ais/vessel_trends.RData')



