library(ggplot2)
library(dplyr)

pcoll <- readRDS('data-raw/collision_fig.RData')
pmort <- readRDS('data-raw/lethal_fig.RData')

names(pcoll) <- c('Speed (kn)', 'P', 'Vessel group')
pcoll$curve <- 'P(Collision)'

names(pmort) <- c('Speed (kn)', 'P', 'Vessel group')
pmort$curve <- 'P(Lethality)'

pcoll %>% head
pmort %>% head

mr <- rbind(pcoll, pmort)
keeper <- mr$`Vessel group` %>% unique %>% tail(1)

mr <- mr %>% filter(`Vessel group` == keeper)

ggplot(mr, aes(x=`Speed (kn)`,
               y=P)) +
               #color=`Vessel group`)) +
  geom_line(lwd=1.2, alpha=.75, col='firebrick') +
  scale_x_continuous(breaks=seq(0,30,by=5)) +
  scale_y_continuous(breaks=seq(0,1,by=.1), limits=c(0,1)) +
  theme_light() +
  facet_wrap(~curve)

ggsave('tests/figs/avoid-lethal/Fig-avoid-lethal.png', width=6, height=3)
