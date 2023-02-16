################################################################################
# maps
################################################################################

library(dplyr)
library(ggplot2)
library(bangarang)
library(devtools)
library(truncnorm)
library(PBSmapping)
library(RColorBrewer)

load_all()

data(grid_kfs)
head(grid_kfs)

display.brewer.all()

################################################################################
################################################################################
################################################################################
# Mapping functions

grid_counter <- function(grid_kfs, traffic){
  gridn <- grid_kfs
  gridn$n <- 0
  i=1
  for(i in 1:nrow(gridn)){
    gridi <- gridn[i,]
    (n <- nrow(traffic[traffic$grid_id == gridi$id,]))
    gridn$n[i] <- n
  }
  return(gridn)
}

varscale <- function(gridr,
                     v,
                     palette = brewer.pal(n=9, name='YlGnBu'),
                     breaks = NULL,
                     var_round = 0){
  #gridr <- grid_kfs
  #v = 'd_mean'
  #(palette <- brewer.pal(n=9, name='Blues'))
  #var_round <- 0

  (varcol <- gridr[,v])
  if(is.null(breaks)){
    i=1
    q <- c()
    for(i in 1:length(varcol)){
      (varcoli <- varcol[i])
      q[i] <- (length(which(varcol < varcoli))) / length(varcol)
    }
    gridr$varcol <- varcol
    gridr$q <- q
    gridr$coli <- ceiling((q + 0.00001) * length(palette))
    #gridr$coli %>% table
    gridr$col <- palette[gridr$coli]

    # Prep key
    i=1
    key <- data.frame()
    for(i in 1:length(palette)){
      (pali <- palette[i])
      (qi <- (i-1)/length(palette))
      vari <- quantile(varcol, qi)
      keyi <- data.frame(col = pali, var = vari)
      key <- rbind(key, keyi)
    }
    key
    key$var <- round(key$var, var_round)
    key

  }else{
    #breaks <- c(0, 0.0001, 0.001, 0.025, 0.050, 0.075, 0.10, 0.125, 0.15)
    (coli <- rep(1, times=length(varcol)))
    for(i in 1:length(breaks)){
      coli[varcol > breaks[i]] <- i
    }
    coli %>% table
    #coli <- cut(varcol, breaks, labels=FALSE)
    #coli[is.na(coli)] <- length(palette)
    gridr$col <- palette[coli]
    (key <- data.frame(col=palette, var = breaks))
  }

  head(gridr)

  return(list(grid = gridr, key = key))
}


varmap <- function(gridr,
                   tit,
                   key_prefix = '',
                   key_suffix = '',
                   tit_cex = 1.4,
                   key_cex = 1,
                   poly_right = -129.4,
                   poly_bottom = 53.49,
                   show_key = TRUE,
                   to_file = TRUE,
                   map_dir = 'tests/figs/maps/'){

  #tit <- 'Title'
  #poly_right = -129.4
  #poly_bottom = 53.495
  #map_dir = 'tests/fw/impacts_2019/maps/'
  #to_file <- TRUE
  #tit_cex = 1.4
  #key_prefix = '> '
  #key_suffix = ' m'

  (key <- gridr$key)
  gridr <- gridr$grid

  (fn <- paste0(map_dir, tit, '.pdf'))

  if(to_file){
    pdf(file=fn, width=6, height=8.5)
  }

  data(nepacLLhigh)
  long <- c(-129.68, -128.85)
  lat <- c(52.8, 53.55)
  land.col = "light gray"
  shore.border = "dark gray"
  shore.lwd = 1

  par(mar=c(3.5,3.25,.5,.25))
  PBSmapping::plotMap(nepacLLhigh, xlim = long, ylim = lat, col = land.col,
                      border = shore.border, lwd = shore.lwd, plt = NULL)

  points(x=gridr$x,
         y=gridr$y,
         pch=15,
         cex=1.65,
         col=gridr$col)

  addPolys(nepacLLhigh, border=shore.border, col=land.col, lwd= 1.5)

  polygon(x=c(-129.8, poly_right, poly_right, -129.8),
          y=c(poly_bottom, poly_bottom, 53.8, 53.8),
          col=adjustcolor('white',alpha.f=.5),
          border=NA)

  # Title
  text(x=-129.67, y=53.52, labels=tit, cex=tit_cex, pos=4)

  # Key
  if(show_key){
    legend(x=-129.05,
           y=53.025,
           legend = paste0(key_prefix,key$var,key_suffix),
           cex = 1,
           col = key$col,
           pch = 15,
           pt.cex = 1.65,
           box.lwd = 0,
           bg = adjustcolor('white',alpha.f=1))
  }


  if(to_file){dev.off()}

  message(fn)
}


################################################################################
################################################################################
################################################################################
################################################################################
# Spatial grid

grid_kfs %>% head

gridr <- varscale(grid_kfs, 'z')
varmap(gridr,
       tit = 'Seafloor depth',
       key_prefix = '> ',
       key_suffix = ' m',
       poly_right = -129.365,
       poly_bottom = 53.495)


(gridr <- varscale(grid_kfs, 'zrange'))
varmap(gridr,
       tit = 'Seafloor range',
       key_prefix = '> ',
       key_suffix = ' m',
       poly_right = -129.365,
       poly_bottom = 53.495)

################################################################################
################################################################################
################################################################################

# Traffic
data(ais_2019)
traffic <- ais_2019
traffic %>% head
(vessels <- unique(traffic$type))
(channels <- unique(traffic$channel))

traffic$channi <- traffic$channel
traffic$channi[traffic$channi == 'WRI'] <- 'Wright'
traffic$channi[traffic$channi == 'WHA'] <- 'Whale'
traffic$channi[traffic$channi == 'VER'] <- 'Verney'
traffic$channi[traffic$channi == 'SQU'] <- 'Squally'
traffic$channi[traffic$channi == 'MCK'] <- 'McKay'
traffic$channi[traffic$channi == 'EST'] <- 'Estevan'
traffic$channi[traffic$channi == 'CMP'] <- 'Campania'
traffic$channi[traffic$channi == 'CAA'] <- 'Caamano'
traffic$channi %>% table
traffic$channi <- factor(traffic$channi, levels=c('Caamano','Estevan','Campania','Squally',
                                                  'Whale','Wright','McKay','Verney'))

months <- c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec')
months <- factor(months, levels=months)
traffic$m <- months[traffic$month]
traffic$m %>% table

# Map ==========================================================================

# All traffic
gridn <- grid_counter(grid_kfs, traffic)
(gridr <- varscale(gridn, 'n', palette = brewer.pal(n=9, name='BuPu')))
varmap(gridr, tit = 'Vessel transits (2019)',
       key_prefix = '> ', key_suffix = '',
       poly_right = -129.26,
       poly_bottom = 53.495)


# Summarize by month & channel =========================================================

tfi <-
  traffic %>%
  mutate(dt_vid = paste0(vid,'-',lubridate::yday(datetime))) %>%
  group_by(channi, month, m) %>%
  summarize(n = length(unique(dt_vid)))

head(tfi)

ggplot(tfi, aes(x=month, y=n, color=channi, lty=channi)) +
  geom_line() +
  scale_x_continuous(breaks=1:12, labels=months) +
  xlab(NULL) +
  ylab('Transits') +
  labs(color = 'Channel', lty='Channel') +
  theme_light()


ggsave(filename='tests/figs/ais/monthly-channels.png', width=9, height=3)


# Summarize by month & vessel type =========================================================

tfi <-
  traffic %>%
  mutate(dt_vid = paste0(vid,'-',lubridate::yday(datetime))) %>%
  group_by(type, month, m) %>%
  summarize(n = length(unique(dt_vid)))

head(tfi)

ggplot(tfi, aes(x=month, y=n, color=type, lty=type)) +
  geom_line() +
  scale_x_continuous(breaks=1:12, labels=months) +
  xlab(NULL) +
  ylab('Transits') +
  labs(color = 'Vessel type', lty='Vessel type') +
  theme_light()

ggsave(filename='tests/figs/ais/monthly-vessels.png', width=9, height=3)


################################################################################
################################################################################
################################################################################
################################################################################

# FIN Whale density
load('tests/fw/dsm-estimate.RData')
whales <- grids
whales %>% head

whales$d_mean[whales$d_mean < 0] <- 0
whales$d_mean %>% hist

breaks <- c(0, 0.005, 0.01, 0.02, 0.05, 0.08)

(gridr <- varscale(whales, 'd_mean',
                   palette = brewer.pal(n=6, name='PuBuGn'),
                   breaks = breaks,
                   var_round = 5))

varmap(gridr, tit = 'Fin whale density',
       key_prefix = '>= ', key_suffix = '',
       poly_right = -129.3,
       poly_bottom = 53.495)



################################################################################
################################################################################
################################################################################
################################################################################

# HUMPBACK Whale density
load('tests/hw/dsm-estimate.RData')
whales <- grids
whales %>% head
whales$month %>% table

which(whales$d_mean < 0)
whales$d_mean[whales$d_mean < 0] <- 0
whales$d_mean %>% hist

whales %>%
  group_by(month) %>%
  summarize(D = mean(d_mean))

whales %>%
  group_by(block) %>%
  summarize(D = mean(d_mean))

# Overall ======================================================================

# Overall
breaks <- c(0, 0.025,  0.05,   0.75,  0.1,   .15,  .2)
whali <- whales %>% dplyr::filter(month == 0)
hist(whali$d_mean)
(gridr <- varscale(whali, 'd_mean',
                   palette = brewer.pal(n=7, name='PuBuGn'),
                   breaks = breaks, var_round = 5))

varmap(gridr, tit = 'Humpback whale density',
       key_prefix = '>= ', key_suffix = '', poly_right = -129.2, poly_bottom = 53.495)

# June
#breaks <- c(0, 0.00025, 0.0005, 0.001, 0.002, 0.004, 0.006)
whali <- whales %>% dplyr::filter(month == 6)
hist(whali$d_mean)
(gridr <- varscale(whali, 'd_mean',
                   palette = brewer.pal(n=7, name='PuBuGn'),
                   breaks = breaks, var_round = 5))
varmap(gridr, tit = 'Humpback whale - June',
       key_prefix = '>= ', key_suffix = '', poly_right = -129.2, poly_bottom = 53.495, show_key = FALSE)

# July
whali <- whales %>% dplyr::filter(month == 7)
hist(whali$d_mean)
(gridr <- varscale(whali, 'd_mean',
                   palette = brewer.pal(n=7, name='PuBuGn'),
                   breaks = breaks, var_round = 5))
varmap(gridr, tit = 'Humpback whale - July',
       key_prefix = '>= ', key_suffix = '', poly_right = -129.2, poly_bottom = 53.495, show_key = FALSE)

# August
whali <- whales %>% dplyr::filter(month == 8)
hist(whali$d_mean)
(gridr <- varscale(whali, 'd_mean',
                   palette = brewer.pal(n=7, name='PuBuGn'),
                   breaks = breaks, var_round = 5))
varmap(gridr, tit = 'Humpback whale - August',
       key_prefix = '>= ', key_suffix = '', poly_right = -129.2, poly_bottom = 53.495, show_key = FALSE)

# September
whali <- whales %>% dplyr::filter(month == 9)
hist(whali$d_mean)
(gridr <- varscale(whali, 'd_mean',
                   palette = brewer.pal(n=7, name='PuBuGn'),
                   breaks = breaks, var_round = 5))
varmap(gridr, tit = 'Humpback whale - September',
       key_prefix = '>= ', key_suffix = '', poly_right = -129.2, poly_bottom = 53.495, show_key = FALSE)


# Whale density bootstraps =====================================================

load('tests/hw/dsm-bootstraps.RData')
boots <- bootstraps
boots %>% head

gridjoin <- grid_kfs %>% dplyr::select(grid_id=id,block)
boots <- left_join(boots, gridjoin, by='grid_id')
head(boots)

hist(boots$D)

which(boots$D > 50)
which(is.na(boots$D))
boots[4220:4230,]

# Overall
boots %>%
  group_by(month) %>%
  summarize(d = round(mean(D, na.rm=TRUE), 3),
            lci = round(quantile(D, .05, na.rm=TRUE),3),
            uci = round(quantile(D, .95, na.rm=TRUE),3)) %>%
  mutate(d = ifelse(d < 0.0000001, 0, d),
         lci = ifelse(lci < 0.0000001, 0, lci),
         uci = ifelse(uci < 0.0000001, 0, uci))

boots %>%
  dplyr::filter(month == 0) %>%
  group_by(block) %>%
  summarize(d = round(mean(D, na.rm=TRUE), 3),
            lci = round(quantile(D, .05, na.rm=TRUE),3),
            uci = round(quantile(D, .95, na.rm=TRUE),3)) %>%
  mutate(d = ifelse(d < 0.0000001, 0, d),
         lci = ifelse(lci < 0.0000001, 0, lci),
         uci = ifelse(uci < 0.0000001, 0, uci))

# Each block in each month
  boots %>%
    dplyr::filter(month != 0) %>%
    group_by(month, block) %>%
  summarize(d = round(mean(D, na.rm=TRUE), 3),
            lci = round(quantile(D, .05, na.rm=TRUE),3),
            uci = round(quantile(D, .95, na.rm=TRUE),3)) %>%
  mutate(d = ifelse(d < 0.0000001, 0, d),
         lci = ifelse(lci < 0.0000001, 0, lci),
         uci = ifelse(uci < 0.0000001, 0, uci)) %>%
    as.data.frame()



################################################################################
################################################################################
################################################################################
################################################################################
# FIN Whale seasonality

months <- c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec')
months <- factor(months, levels=months)

# Seasonal posterior
load('tests/fw/seasonal_posterior.RData') %>% View
seasonal_boot %>% head

seasonal_boot$m <- months[seasonal_boot$month]

ggplot(seasonal_boot, aes(x=m, y=scaled)) +
  geom_violin()

# Add actual density
load('tests/fw/dsm-bootstraps.RData')
df <- bootstraps
head(df)
df$month %>% table

seasonal_boot %>% head
seasonal_boot$month %>% table
df %>% head

season_boots <- data.frame()
months <- 1:12
mi=7
for(mi in 1:length(months)){
  (monthi <- months[mi])
  print(monthi)
  seasi <- seasonal_boot %>% dplyr::filter(month == monthi)
  nrow(seasi)
  head(seasi)
  hist(seasi$scaled)

  dfi <- df
  dfi$D <- dfi$D*sample(seasi$scaled, size=nrow(dfi), replace=TRUE)
  head(dfi)
  dfi$month <- mi
  dfi$m <- monthi
  season_boots <- rbind(season_boots, dfi)
}

nrow(season_boots)

head(season_boots)
season_boots$month %>% table


# Deal with forgetting to do iteration count properly
mr <- data.frame()
mi = 1
for(mi in 1:length(months)){
  (monthi <- months[mi])
  print(monthi)
  booti <- season_boots %>% dplyr::filter(m == monthi)
  nrow(booti)
  head(booti)

  #(booti$grid_id %>% table %>% table)
  (iterations <- 1000)
  bootii <-
    booti %>%
    group_by(grid_id) %>%
    mutate(iteration = 1:iterations)

  bootiii <-
    bootii %>%
    group_by(iteration) %>%
    summarize(D = mean(D)) %>%
    mutate(month = mi,
           m = monthi) %>%
    dplyr::select(month, m, iteration, D)

  bootiii %>% head
  # hist(bootiii$D)
  mr <- rbind(mr, bootiii)
}

mr %>% nrow
mr %>% head

booti <-
  mr %>%
  #seasonal_boot %>%
  group_by(month, m) %>%
  summarize(q.2.5 = quantile(D, 0.025),
            q.5 = quantile(D, 0.05),
            q.10 = quantile(D, 0.1),
            q.25 = quantile(D, 0.25),
            q.50 = quantile(D, 0.5),
            q.75 = quantile(D, 0.75),
            q.90 = quantile(D, 0.9),
            q.95 = quantile(D, 0.95),
            q.975 = quantile(D, 0.975))

booti

d2.5 <- data.frame(month = c(booti$month, rev(booti$month)),
                  q.50 = c(booti$q.2.5, rev(booti$q.975)))
d5 <- data.frame(month = c(booti$month, rev(booti$month)),
                   q.50 = c(booti$q.5, rev(booti$q.95)))
d10 <- data.frame(month = c(booti$month, rev(booti$month)),
                 q.50 = c(booti$q.10, rev(booti$q.90)))
d20 <- data.frame(month = c(booti$month, rev(booti$month)),
                  q.50 = c(booti$q.25, rev(booti$q.75)))

gg_fw_seasonal <-
  ggplot(booti, aes(x=month, y=q.50)) +
  geom_rect(aes(xmin=-Inf, xmax=4.75, ymin=-Inf, ymax=Inf), alpha=.01, fill='steelblue3') +
  geom_rect(aes(xmin=10.75, xmax=Inf, ymin=-Inf, ymax=Inf), alpha=.01, fill='steelblue3') +
  geom_polygon(data=d2.5, mapping=aes(x=month, y=q.50), fill='grey40',alpha=.1) +
  geom_polygon(data=d5, mapping=aes(x=month, y=q.50), fill='grey30',alpha=.125) +
  geom_polygon(data=d10, mapping=aes(x=month, y=q.50), fill='grey20',alpha=.15) +
  geom_polygon(data=d20, mapping=aes(x=month, y=q.50), fill='grey10',alpha=.15) +
  geom_line(lwd=0.8, alpha=1.0) +
  geom_abline(slope=0, intercept=1, lty=3, color='steelblue3') +
  scale_x_continuous(breaks=1:12, labels=months) +
  scale_y_continuous(n.breaks=8, limits=c(0,0.025)) +
  labs(y = expression(paste("Fin whales ", km^{-2})), x = 'Month') +
  theme_light() +
  theme(panel.grid.minor = element_blank())

gg_fw_seasonal

################################################################################
################################################################################
################################################################################
# Humpback whale seasonality

df <- readRDS('tests/hw/pwhale_seasonal_boots.RData')

# Deal with forgetting to do iteration count properly
mr <- data.frame()
(monthnos <- unique(df$month))
mi = 1
for(mi in 1:length(monthnos)){
  (monthi <- monthnos[mi])
  print(monthi)
  booti <- df %>% dplyr::filter(month == monthi)
  nrow(booti)
  head(booti)

  #(booti$grid_id %>% table %>% table)
  (iterations <- 5000)
  bootii <-
    booti %>%
    group_by(grid_id) %>%
    mutate(iteration = 1:iterations)

  bootiii <-
    bootii %>%
    group_by(iteration) %>%
    summarize(D = mean(D)) %>%
    mutate(month = monthi) %>%
    dplyr::select(month, iteration, D)

  bootiii %>% head
  # hist(bootiii$D)
  mr <- rbind(mr, bootiii)
}

mr %>% nrow
mr %>% head

ggplot(mr, aes(x=factor(month), y=D)) +
  geom_violin()

mrs <-
  mr %>%
  group_by(month) %>%
  summarize(q.50 = median(D),
            q.2.5 = quantile(D, .025),
            q.5 = quantile(D, .05),
            q.10 = quantile(D, .10),
            q.25 = quantile(D, .25),
            q.75 = quantile(D, .75),
            q.90 = quantile(D, .90),
            q.95 = quantile(D, .95),
            q.975 = quantile(D, .975))

d2.5 <- data.frame(month = c(mrs$month, rev(mrs$month)),
                   q.50 = c(mrs$q.2.5, rev(mrs$q.975)))
d5 <- data.frame(month = c(mrs$month, rev(mrs$month)),
                 q.50 = c(mrs$q.5, rev(mrs$q.95)))
d10 <- data.frame(month = c(mrs$month, rev(mrs$month)),
                  q.50 = c(mrs$q.10, rev(mrs$q.90)))
d20 <- data.frame(month = c(mrs$month, rev(mrs$month)),
                  q.50 = c(mrs$q.25, rev(mrs$q.75)))


gg_hw_seasonal <-
  ggplot(mrs, aes(x=month, y=q.50)) +
  geom_rect(aes(xmin=-Inf, xmax=5.5, ymin=-Inf, ymax=Inf), alpha=.01, fill='steelblue3') +
  geom_rect(aes(xmin=10, xmax=Inf, ymin=-Inf, ymax=Inf), alpha=.01, fill='steelblue3') +
  geom_polygon(data=d2.5, mapping=aes(x=month, y=q.50), fill='grey40',alpha=.1) +
  geom_polygon(data=d5, mapping=aes(x=month, y=q.50), fill='grey30',alpha=.125) +
  geom_polygon(data=d10, mapping=aes(x=month, y=q.50), fill='grey20',alpha=.15) +
  geom_polygon(data=d20, mapping=aes(x=month, y=q.50), fill='grey10',alpha=.15) +
  geom_line(lwd=.8, alpha=1.0) +
  scale_x_continuous(breaks=1:12, labels=months) +
  scale_y_continuous(n.breaks=8) +
  labs(y = expression(paste("Humpback whales ", km^{-2})), x = 'Month') +
  theme_light() +
  theme(panel.grid.minor = element_blank())

gg_hw_seasonal

################################################################################
################################################################################
################################################################################
# Combine the two species' plots

library(ggpubr)
#tiff(file='tests/figs/seasonal.tiff', res=150, width=(9*150), height=(3*150))
pdf(file='tests/figs/seasonal.pdf', width=9, height=3)
ggarrange(gg_fw_seasonal, gg_hw_seasonal, nrow=1, labels='auto')
dev.off()


