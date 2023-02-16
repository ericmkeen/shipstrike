################################################################################
################################################################################
# Prep other years of AIS traffic
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
################################################################################
# 2014

getwd()
fn <- '/Users/erickeen/Dropbox/Other WIPs/NCCS/Projects/ship-strike/data/ais/HartleyB2014.csv'
df <- read_csv(fn)

################################################################################
# Review

nrow(df)
head(df)
df$Local.Time %>% range(na.rm=TRUE)
df$Latitude %>% range
df$Longitude %>% range

# plotKFS()
# keeps <- rep(TRUE, times=nrow(df))
# keeps[sample(1:nrow(df), size=round(.5*nrow(df)))] <- FALSE
# dfi <- df[keeps,]
# points(x=dfi$Longitude,
#        y=dfi$Latitude,
#        cex=.1, pch=16)

################################################################################
# Filter & format AIS data

# get lat/long range of analysis grid
data(grid_kfs, package='shipstrike')
grid_kfs$y %>% range
grid_kfs$x %>% range

head(df)

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
  dplyr::select(vid = ID,
                type = Type,
                speed = SOG,
                length = Length,
                width = Beam,
                draft = Draught,
                datetime = Local.Time,
                x = Longitude,
                y = Latitude) %>%
  dplyr::mutate(datetime = lubridate::ymd_hms(datetime, tz='Canada/Pacific'))

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


################################################################################
# Summarize AIS data

# Group into fewer categories

ais$type %>% table
(types <- ais$type %>% unique)

newtype <- rep('Other < 40m',times=nrow(ais))
newtype[ais$length > 40] <- 'Other > 40m'
newtype[ais$length > 100] <- 'Other > 100m'
newtype[ais$type %in% types[c(11)] & ais$length < 40] <- 'Pleasurecraft < 40m'
newtype[ais$type %in% types[c(4, 6)] & ais$length < 50] <- 'Towing < 50m'
newtype[ais$type %in% types[c(3)] & ais$length < 50] <- 'Tug < 50m'
newtype[ais$type %in% types[c(2)] & ais$length >= 180] <- 'Passenger > 180m'
newtype[ais$type %in% types[c(8)] & ais$length >= 180] <- 'Cargo > 180m'
newtype[ais$type %in% types[c(15)]] <- 'Sailing'
newtype[ais$type %in% types[c(10)] & ais$length < 60] <- 'Fishing < 60m'
newtype[ais$type %in% types[c(12, 19)] & ais$length > 180] <- 'Tanker > 180m'

newtype %>% table

# Update with new type categories
ais$type <- newtype

# Plot vessel characteristics
ggplot(ais, aes(x=length)) +
  geom_histogram() +
  facet_wrap(~type, scales='free') +
  ylab('AIS records') +
  xlab('Reported length (m)')

ggplot(ais, aes(x=speed)) +
  geom_histogram() +
  facet_wrap(~type, scales='free') +
  ylab('AIS records') +
  xlab('Reported speed (knots)')

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

# Save it
saveRDS(vgrid2, file='data-raw/vessels_2014.RData')

ais_2014 <- readRDS('data-raw/vessels_2014.RData')
usethis::use_data(ais_2014, overwrite = TRUE)

################################################################################
################################################################################
################################################################################
# 2015

getwd()
fn <- '/Users/erickeen/Dropbox/Other WIPs/NCCS/Projects/ship-strike/data/ais/HartleyB2015.csv'
df <- read_csv(fn)

################################################################################
# Review

nrow(df)
head(df)
df$Local.Time %>% range(na.rm=TRUE)
df$Latitude %>% range
df$Longitude %>% range

# plotKFS()
# keeps <- rep(TRUE, times=nrow(df))
# keeps[sample(1:nrow(df), size=round(.5*nrow(df)))] <- FALSE
# dfi <- df[keeps,]
# points(x=dfi$Longitude,
#        y=dfi$Latitude,
#        cex=.1, pch=16)

################################################################################
# Filter & format AIS data

# get lat/long range of analysis grid
data(grid_kfs, package='shipstrike')
grid_kfs$y %>% range
grid_kfs$x %>% range

head(df)

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
  dplyr::select(vid = ID,
                type = Type,
                speed = SOG,
                length = Length,
                width = Beam,
                draft = Draught,
                datetime = Local.Time,
                x = Longitude,
                y = Latitude) %>%
  dplyr::mutate(datetime = lubridate::ymd_hms(datetime, tz='Canada/Pacific'))

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


################################################################################
# Summarize AIS data

# Group into fewer categories

ais$type %>% table
(types <- ais$type %>% unique)

newtype <- rep('Other < 40m',times=nrow(ais))
newtype[ais$length > 40] <- 'Other > 40m'
newtype[ais$length > 100] <- 'Other > 100m'
newtype[ais$type %in% types[c(10)] & ais$length < 40] <- 'Pleasurecraft < 40m'
newtype[ais$type %in% types[c(1, 4)] & ais$length < 50] <- 'Towing < 50m'
newtype[ais$type %in% types[c(5)] & ais$length < 50] <- 'Tug < 50m'
newtype[ais$type %in% types[c(2)] & ais$length >= 180] <- 'Passenger > 180m'
newtype[ais$type %in% types[c(7, 14, 16)] & ais$length >= 180] <- 'Cargo > 180m'
newtype[ais$type %in% types[c(12)]] <- 'Sailing'
newtype[ais$type %in% types[c(3)] & ais$length < 60] <- 'Fishing < 60m'
newtype[ais$type %in% types[c(9, 15)] & ais$length > 180] <- 'Tanker > 180m'

newtype %>% table

# Update with new type categories
ais$type <- newtype

# Plot vessel characteristics
ggplot(ais, aes(x=length)) +
  geom_histogram() +
  facet_wrap(~type, scales='free') +
  ylab('AIS records') +
  xlab('Reported length (m)')

ggplot(ais, aes(x=speed)) +
  geom_histogram() +
  facet_wrap(~type, scales='free') +
  ylab('AIS records') +
  xlab('Reported speed (knots)')

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

# Save it
saveRDS(vgrid2, file='data-raw/vessels_2015.RData')

ais_2015 <- readRDS('data-raw/vessels_2015.RData')
usethis::use_data(ais_2015, overwrite = TRUE)

################################################################################
################################################################################
################################################################################
# 2018

getwd()
fn <- '/Users/erickeen/Dropbox/Other WIPs/NCCS/Projects/ship-strike/data/ais/HartleyB2018.csv'
df <- read_csv(fn)

################################################################################
# Review

nrow(df)
head(df)
df$Local.Time %>% range(na.rm=TRUE)
df$Latitude %>% range
df$Longitude %>% range

# plotKFS()
# keeps <- rep(TRUE, times=nrow(df))
# keeps[sample(1:nrow(df), size=round(.5*nrow(df)))] <- FALSE
# dfi <- df[keeps,]
# points(x=dfi$Longitude,
#        y=dfi$Latitude,
#        cex=.1, pch=16)

################################################################################
# Filter & format AIS data

# get lat/long range of analysis grid
data(grid_kfs, package='shipstrike')
grid_kfs$y %>% range
grid_kfs$x %>% range

head(df)

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
  dplyr::select(vid = ID,
                type = Type,
                speed = SOG,
                length = Length,
                width = Beam,
                draft = Draught,
                datetime = Local.Time,
                x = Longitude,
                y = Latitude) %>%
  dplyr::mutate(datetime = lubridate::ymd_hms(datetime, tz='Canada/Pacific'))

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


################################################################################
# Summarize AIS data

# Group into fewer categories

ais$type %>% table
(types <- ais$type %>% unique)

newtype <- rep('Other < 40m',times=nrow(ais))
newtype[ais$length > 40] <- 'Other > 40m'
newtype[ais$length > 100] <- 'Other > 100m'
newtype[ais$type %in% types[c(5)] & ais$length < 40] <- 'Pleasurecraft < 40m'
newtype[ais$type %in% types[c(6)] & ais$length < 50] <- 'Towing < 50m'
newtype[ais$type %in% types[c(4)] & ais$length < 50] <- 'Tug < 50m'
newtype[ais$type %in% types[c(7)] & ais$length >= 180] <- 'Passenger > 180m'
newtype[ais$type %in% types[c(2, 17)] & ais$length >= 180] <- 'Cargo > 180m'
newtype[ais$type %in% types[c(13)]] <- 'Sailing'
newtype[ais$type %in% types[c(3)] & ais$length < 60] <- 'Fishing < 60m'
newtype[ais$type %in% types[c(12)] & ais$length > 180] <- 'Tanker > 180m'

newtype %>% table

# Update with new type categories
ais$type <- newtype

# Plot vessel characteristics
ggplot(ais, aes(x=length)) +
  geom_histogram() +
  facet_wrap(~type, scales='free') +
  ylab('AIS records') +
  xlab('Reported length (m)')

ggplot(ais, aes(x=speed)) +
  geom_histogram() +
  facet_wrap(~type, scales='free') +
  ylab('AIS records') +
  xlab('Reported speed (knots)')

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

# Save it
saveRDS(vgrid2, file='data-raw/vessels_2018.RData')

ais_2018 <- readRDS('data-raw/vessels_2018.RData')
usethis::use_data(ais_2018, overwrite = TRUE)

