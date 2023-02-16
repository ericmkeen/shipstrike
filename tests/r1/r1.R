# Code scratch for revision 1

library(dplyr)
library(ggplot2)
library(shipstrike)
library(plotly)
data(ais_2014)
data(ais_2015)
data(ais_2018)
data(ais_2019)

ais <- rbind(ais_2014, ais_2015, ais_2018, ais_2019)

ais$type %>% unique
ais20 <- ais %>% filter(type == 'Pleasurecraft < 40m',
                             length < 20)
ais20 %>% nrow

ais40 <- ais %>% filter(type == 'Pleasurecraft < 40m',
                             length >= 20)
ais40 %>% nrow

ais20$year %>% unique
ais40$year %>% unique

ais


# Did vessels cross land?

data(ais_2019)
head(ais_2019)

(vids <- ais_2019$vid %>% unique)
aisi <- ais_2019 %>% filter(vid == vids[620])
gg_kfs() +
  geom_path(data=aisi,
           aes(x=x, y=y,
               color=factor(lubridate::as_date(datetime))))
#ggplotly()

ais_2019 %>%
  mutate(datetime = lubridate::as_datetime(datetime)) %>%
  mutate(date = lubridate::as_date(datetime)) %>%
  group_by(vid, date) %>%
  mutate(dt_prev = lag(datetime),
         dt_lag = difftime(dt_prev, datetime, units='secs')) %>%
  ungroup() %>%
  mutate(dt_lag = abs(as.numeric(dt_lag))) %>%
  filter(is.finite(dt_lag)) %>%
  filter(abs(dt_lag) < 1000) %>%
  pull(dt_lag) %>%
  median()
  hist(breaks=seq(0, 1100, by=10))

