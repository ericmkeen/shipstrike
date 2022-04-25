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
which(is.na(ais$width))
plot(width ~ length, data=ais, pch=16, cex=.5)
# none look invalid

# Find invalid drafts
# any draft 50% of length or deeper is invalid
ld_ratio <- ais$draft / ais$length
max(ld_ratio, na.rm=TRUE)
bads <- which(ld_ratio > 0.5)
ais$draft[bads] <- NA
(bads <- which(is.na(ais$draft))) %>% length

# Fix invalid drafts
# Use a 1-to-0.05 ratio
ais$draft[bads] <- ais$length[bads] * 0.05
plot(draft ~ length, data=ais, pch=16, cex=.5)


################################################################################
################################################################################
# Assign AIS to provinces

#data('channels', package='bangarang')
#head(channels)
#channels$province %>% table
#plotKFS()
#lines(x=channels$X[channels$province=='nsq'],
#       y=channels$Y[channels$province=='nsq'])
#
#(blox <- channels$province %>% unique)
#i=1
#for(i in 1:length(blox)){
#  (bloxi <- blox[i])
#
#}

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
            transits_per_day = round(dates / 365, 2),
            speed_mean = mean(speed, na.rm=TRUE) %>% round(1),
            speed_sd = sd(speed, na.rm=TRUE) %>% round(1),
            speed_max = max(speed, na.rm=TRUE) %>% round(1),
            length_mean = mean(length, na.rm=TRUE) %>% round(),
            length_sd = sd(length, na.rm=TRUE) %>% round(),
            length_min = min(length, na.rm=TRUE) %>% round(),
            length_max = max(length, na.rm=TRUE) %>% round())

ais_table %>% View

ais$type %>% table
(types <- ais$type %>% unique)

newtype <- rep('Other',times=nrow(ais))
newtype[ais$type %in% types[c(1, 12, 20)]] <- 'Government'
newtype[ais$type %in% types[c(11, 19)]] <- 'Pilot/Port'
newtype[ais$type %in% types[c(4, 7)]] <- 'Towing'
newtype[ais$type %in% types[c(3)]] <- 'Tug'
newtype[ais$type %in% types[c(2, 17)]] <- 'Passenger < 100m'
newtype[ais$type %in% types[c(2, 17)] & ais$length >= 100] <- 'Passenger > 100m'
newtype[ais$type %in% types[c(5, 21)]] <- 'Cargo < 100m'
newtype[ais$type %in% types[c(5, 21)] & ais$length >= 100] <- 'Cargo > 100m'
newtype[ais$type %in% types[c(14)]] <- 'Tanker < 100m'
newtype[ais$type %in% types[c(14)] & ais$length >= 100] <- 'Tanker > 100m'
newtype[ais$type %in% types[c(13)]] <- 'Sailing'
newtype[ais$type %in% types[c(6)]] <- 'Fishing'

newtype %>% table

# Update with new type categories
ais$type <- newtype

# Update table
ais_table <-
  ais %>%
  group_by(type) %>%
  summarize(vids = unique(vid) %>% length,
            fixes = n(),
            dates = lubridate::yday(datetime) %>% unique %>% length,
            transits_per_day = round(dates / 365, 2),
            speed_mean = mean(speed, na.rm=TRUE) %>% round(1),
            speed_sd = sd(speed, na.rm=TRUE) %>% round(1),
            speed_max = max(speed, na.rm=TRUE) %>% round(1),
            length_mean = mean(length, na.rm=TRUE) %>% round(),
            length_sd = sd(length, na.rm=TRUE) %>% round(),
            length_min = min(length, na.rm=TRUE) %>% round(),
            length_max = max(length, na.rm=TRUE) %>% round())

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
vgrid2$diel[vgrid2$sun < 0] <- 'night'
vgrid2$diel %>% table

################################################################################
################################################################################

# Save output
saveRDS(vgrid2, file='tests/vessels_2019.RData')



