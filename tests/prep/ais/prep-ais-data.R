################################################################################
################################################################################
# AIS data processing
################################################################################
################################################################################

library(readr)
library(dplyr)
library(bangarang)
library(devtools)
library(lubridate)
library(ggplot2)

document()

################################################################################
################################################################################

getwd()
fn <- '/Users/erickeen/Dropbox/Other WIPs/NCCS/Projects/ship-strike/data/ais/AIS_2019_w_month.csv'
df <- read_csv(fn)

################################################################################
################################################################################

nrow(df)
head(df)
df$Local.Time %>% range(na.rm=TRUE)
df$Latitude %>% range
df$Longitude %>% range

plotKFS()
keeps <- rep(TRUE, times=nrow(df))
keeps[sample(1:nrow(df), size=round(.5*nrow(df)))] <- FALSE
dfi <- df[keeps,]
points(x=dfi$Longitude,
       y=dfi$Latitude,
       cex=.1, pch=16)

################################################################################
################################################################################
# Filter & format AIS data

# get lat/long range of analysis grid
data(grid_kfs, package='shipstrike')
grid_kfs$y %>% range
grid_kfs$x %>% range

# format/filter
ais <-
  df %>%
  dplyr::filter(SOG > 3,
                SOG < 40,
                Length > 5,
                Length < 500) %>%
  dplyr::filter(Longitude >= -129.68,
                Longitude < -128.66,
                Latitude >= 52.8,
                Latitude < 53.55) %>%
  dplyr::select(vid = id,
                type = Type,
                speed = SOG,
                length = Length,
                width = Beam,
                draft = Draught,
                datetime = UTC.Time,
                x = Longitude,
                y = Latitude) %>%
  dplyr::mutate(datetime = lubridate::ymd_hm(datetime, tz='UTC'))

head(ais$datetime)
head(ais)

# Fix invalid beams
(bads <- which(ais$width < 2))
ais$width[bads] <- NA
(bads <- which(is.na(ais$width))) %>% length
if(length(bads)>0){ais$width[bads] <- ais$length[bads] * 0.125}
plot(width ~ length, data=ais, pch=16, cex=.5)

# Find invalid drafts
# any draft 50% of length or deeper is invalid
ld_ratio <- ais$draft / ais$length
max(ld_ratio, na.rm=TRUE)
(bads <- which(ld_ratio > 0.5)) %>% length
if(length(bads)>0){ais$draft[bads] <- NA}
(bads <- which(ais$draft < .2)) %>% length
if(length(bads)>0){ais$draft[bads] <- NA}

# Fix invalid drafts
# Use a 1-to-0.05 ratio
(bads <- which(is.na(ais$draft))) %>% length
if(length(bads)>0){ais$draft[bads] <- ais$length[bads] * 0.05}
plot(draft ~ length, data=ais, pch=16, cex=.5)


(1184 + 118838) / nrow(ais)

################################################################################
################################################################################
# Summarize AIS data

nrow(ais)

ais$vid %>% unique %>% length

ais_table <-
  ais %>%
  group_by(type) %>%
  summarize(vids = unique(vid) %>% length,
            fixes = n(),
            dates = lubridate::yday(datetime) %>% unique %>% length,
            daily_transits = round(dates / 365, 2),
            speed_mn = mean(speed, na.rm=TRUE) %>% round(1),
            speed_sd = sd(speed, na.rm=TRUE) %>% round(1),
            speed_max = max(speed, na.rm=TRUE) %>% round(1),
            length_mn = mean(length, na.rm=TRUE) %>% round(),
            length_sd = sd(length, na.rm=TRUE) %>% round(),
            length_min = min(length, na.rm=TRUE) %>% round(),
            length_max = max(length, na.rm=TRUE) %>% round(),
            beam_mn = mean(width, na.rm=TRUE) %>% round(),
            beam_sd = sd(width, na.rm=TRUE) %>% round(),
            beam_min = min(width, na.rm=TRUE) %>% round(),
            beam_max = max(width, na.rm=TRUE) %>% round(),
            draft_mn = mean(draft, na.rm=TRUE) %>% round(1),
            draft_sd = sd(draft, na.rm=TRUE) %>% round(1),
            draft_min = min(draft, na.rm=TRUE) %>% round(1),
            draft_max = max(draft, na.rm=TRUE) %>% round(1))

ais_table %>% View

# Group into fewer categories

ais$type %>% table
(types <- ais$type %>% unique)

newtype <- rep('Other < 40m',times=nrow(ais))
newtype[ais$length > 40] <- 'Other > 40m'
newtype[ais$type %in% types[c(9)] & ais$length < 40] <- 'Pleasurecraft < 40m'
newtype[ais$type %in% types[c(4, 7)] & ais$length < 50] <- 'Towing < 50m'
newtype[ais$type %in% types[c(3)] & ais$length < 50] <- 'Tug < 50m'
newtype[ais$type %in% types[c(2, 17)] & ais$length >= 100] <- 'Passenger > 100m'
newtype[ais$type %in% types[c(5, 21)] & ais$length >= 100] <- 'Cargo > 100m'
newtype[ais$type %in% types[c(14)] & ais$length >= 100] <- 'Tanker > 100m'
newtype[ais$type %in% types[c(13)]] <- 'Sailing'
newtype[ais$type %in% types[c(6)] & ais$length < 60] <- 'Fishing < 60m'

newtype %>% table

# Update with new type categories
ais$type <- newtype

# Plot vessel characteristics
ggplot(ais, aes(x=length)) +
  geom_histogram() +
  facet_wrap(~type, scales='free') +
  ylab('AIS records') +
  xlab('Reported length (m)') +
  labs(title = 'Lengths (meters) for 2019 vessel types used in analysis')

ggplot(ais, aes(x=speed)) +
  geom_histogram() +
  facet_wrap(~type, scales='free') +
  ylab('AIS records') +
  xlab('Reported speed (knots)') +
  labs(title = 'Speed (knots) for 2019 vessel types used in analysis')

# Update table
ais_table <-
  ais %>%
  group_by(type) %>%
  summarize(vids = unique(vid) %>% length,
            fixes = n(),
            dates = lubridate::yday(datetime) %>% unique %>% length,
            daily_transits = round(dates / 365, 2),
            speed_mean = mean(speed, na.rm=TRUE) %>% round(1),
            speed_sd = sd(speed, na.rm=TRUE) %>% round(1),
            speed_max = max(speed, na.rm=TRUE) %>% round(1),
            length_mean = mean(length, na.rm=TRUE) %>% round(),
            length_sd = sd(length, na.rm=TRUE) %>% round(),
            length_min = min(length, na.rm=TRUE) %>% round(),
            length_max = max(length, na.rm=TRUE) %>% round(),
            beam_mean = mean(width, na.rm=TRUE) %>% round(),
            beam_sd = sd(width, na.rm=TRUE) %>% round(),
            beam_min = min(width, na.rm=TRUE) %>% round(),
            beam_max = max(width, na.rm=TRUE) %>% round(),
            draft_mean = mean(draft, na.rm=TRUE) %>% round(1),
            draft_sd = sd(draft, na.rm=TRUE) %>% round(1),
            draft_min = min(draft, na.rm=TRUE) %>% round(1),
            draft_max = max(draft, na.rm=TRUE) %>% round(1))

ais_table %>% View

################################################################################
################################################################################

# Interpolate the vessel grid
data(grid_kfs, package='shipstrike')
vgrid <- vessel_grid(grid_kfs, ais)

# Join channel to each grid number
nrow(vgrid)
head(vgrid)

head(grid_kfs)
grid_join <- grid_kfs %>% select(grid_id = id, channel = block)
head(grid_join)
vgrid2 <- left_join(vgrid, grid_join, by='grid_id')
head(vgrid2)
vgrid2$channel %>% table

# Add sun angles
vgrid2$sun <- vessel_sun_angle(vgrid2, verbose=TRUE)
vgrid2$diel <- 'day'
vgrid2$diel[vgrid2$sun < -12] <- 'night' # nautical dawn/dusk
vgrid2$diel %>% table

################################################################################
################################################################################

# Save output
saveRDS(vgrid2, file='tests/vessels_2019.RData')

#vess <- readRDS('tests/vessels_2019.RData')
#head(vess)
