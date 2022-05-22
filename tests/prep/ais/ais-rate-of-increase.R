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

ais14 <- readRDS('tests/vessels_2014.RData')
head(ais14)

ais15 <- readRDS('tests/vessels_2015.RData')
head(ais15)

ais18 <- readRDS('tests/vessels_2018.RData')
head(ais18)

ais19 <- readRDS('tests/vessels_2019.RData')
head(ais19)

ais <- rbind(ais14, ais15, ais18, ais19)

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

obs <- pred <- summ <- data.frame()
(types <- ais$type %>% unique)
i=9
for(i in 1:length(types)){
  (typi <- types[i])
  aisi <- ais %>% dplyr::filter(type == typi)

  (aiskm <-
    aisi %>%
    group_by(year) %>%
    summarize(km = sum(km)) %>%
    rename(yyyy = year))

  obsi <- data.frame(type = typi, aiskm) %>% rename(year = yyyy)
  obs <- rbind(obs, obsi)

  (aislm <- lm(km ~ yyyy, data= aiskm)) %>% summary
  (pvali <- summary(aislm)$coefficients[2,4])

  (rati <- summary(aislm)$coefficients[2,1] / obsi$km[obsi$year == 2019])

  (aispred <- data.frame(yyyy=((2030:2039)-3)))
  (aispred$km <- predict(aislm, newdata=aispred))
  aispred$yyyy <- aispred$yyyy + 3
  aispred <-
    aispred %>%
    mutate(type = typi) %>%
    mutate(km_pre = dplyr::lag(km)) %>%
    mutate(diff = abs(km) - abs(km_pre)) %>%
    mutate(rate = diff / km) %>%
    select(type, year = yyyy, km, rate)

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
saveRDS(ais_trends, file='tests/vessel_trends.RData')




