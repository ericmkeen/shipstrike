################################################################################
# LNG Canada
################################################################################

library(bangarang)
library(dplyr)
library(swfscMisc)

document()
data(grid_kfs)
data(tanker_route)
tanker_route

################################################################################
################################################################################

# Total tankers expected  ======================================================
new_calls <- 50

# Dimensions to choose from  ===================================================

(vessel_options <- data.frame(length = c(290, 298, 286, 288, 286),
                              width = c(45, round((298/315)*50), 43, 44, 41),
                              draft = c(12, 12, 11.9, 11.5, 11.8)))

# Get calendar dates of transits ===============================================

set.seed(126)
(spacing <- sample(5:9, size=new_calls, replace=TRUE))
(julians <- cumsum(spacing))
julian_product <- julians + 1

################################################################################
################################################################################
# Loop through each direction

# In-heel  ==================================================================

(traffic <-
    tanker_route %>%
    select(x, y) %>%
    mutate(id = 1:n()) %>%
    arrange(desc(id))) %>%
    head

heels <- data.frame()
t0 <- lubridate::as_datetime('2030-01-01 00:00:01', tz='UTC')
i=1
for(i in 1:length(julians)){
  traffi <- traffic
  head(traffi)

  (start_time <- t0 + lubridate::days(julians[i]) + lubridate::seconds(sample(1:86400, 1)))

  # Add speeds along route
  (speedi <- runif(1,8,14))
  traffi$speed <- speedi

  # add course variability
  (delta_y <- rnorm(1, 0, .002))
  traffi$y <- traffi$y + delta_y
  (delta_x <- rnorm(1, 0, .002))
  traffi$x <- traffi$x + delta_x

  # datetime
  (xy <- data.frame(lat1 = traffi$y[1:(nrow(traffi)-1)],
                    lon1 = traffi$x[1:(nrow(traffi)-1)],
                    lat2 = traffi$y[2:(nrow(traffi))],
                    lon2 = traffi$x[2:(nrow(traffi))]))
  (nmi <- apply(xy, 1, function(x){swfscMisc::distance(x[1],x[2],x[3],x[4], units='nm')}))
  (kms <- nmi * 1.852)
  kms <- c(kms,0.1)
  traffi$km <- kms
  (traffi$hours <- traffi$km / (traffi$speed * 1.852))
  (traffi$tot_hours <- cumsum(traffi$hours))
  traffi$datetime <- start_time + lubridate::seconds(round(3600*traffi$tot_hours))
  traffi$datetime
  head(traffi)

  # Select vessel type
  (vessi <- vessel_options[sample(1:nrow(vessel_options), size = 1),])

  # Format/filter fake AIS data
  ais <-
    traffi %>%
    mutate(type = 'Cedar LNG tanker in-heel',
           length = vessi$length,
           width = vessi$width,
           draft = vessi$draft,
           vid = i) %>%
    dplyr::filter(x >= -129.68,
                  x < -128.66,
                  y >= 52.8,
                  y < 53.55) %>%
    dplyr::select(vid,
                  type,
                  speed,
                  length,
                  width,
                  draft,
                  datetime,
                  x,
                  y)
ais %>% head

heels <- rbind(heels, ais)
message(i)
}

head(heels)
nrow(heels)

# In-Product  ==================================================================

(traffic <-
   tanker_route %>%
   select(x, y) %>%
   mutate(id = 1:n())) %>%
  head

products <- data.frame()
t0 <- lubridate::as_datetime('2030-01-01 00:00:01', tz='UTC')
i=1
for(i in 1:length(julian_product)){
  traffi <- traffic

  (start_time <- t0 + lubridate::days(julian_product[i]) + lubridate::seconds(sample(1:86400, 1)))

  # Add speeds along route
  (speedi <- runif(1,8,14))
  traffi$speed <- speedi

  # add course variability
  (delta_y <- rnorm(1, 0, .002))
  traffi$y <- traffi$y + delta_y
  (delta_x <- rnorm(1, 0, .002))
  traffi$x <- traffi$x + delta_x

  # datetime
  (xy <- data.frame(lat1 = traffi$y[1:(nrow(traffi)-1)],
                    lon1 = traffi$x[1:(nrow(traffi)-1)],
                    lat2 = traffi$y[2:(nrow(traffi))],
                    lon2 = traffi$x[2:(nrow(traffi))]))
  (nmi <- apply(xy, 1, function(x){swfscMisc::distance(x[1],x[2],x[3],x[4], units='nm')}))
  (kms <- nmi * 1.852)
  kms <- c(kms,0.1)
  traffi$km <- kms
  (traffi$hours <- traffi$km / (traffi$speed * 1.852))
  (traffi$tot_hours <- cumsum(traffi$hours))
  traffi$datetime <- start_time + lubridate::seconds(round(3600*traffi$tot_hours))
  traffi$datetime
  head(traffi)

  # Get dimensions based on corresponding in-heel transit
  (heeli <- heels[heels$vid == i, 4:6])
  vessi <- heeli[!duplicated(heeli),]

  # Format/filter fake AIS data
  ais <-
    traffi %>%
    mutate(type = 'Cedar LNG tanker in-product',
           length = vessi$length,
           width = vessi$width,
           draft = vessi$draft,
           vid = i) %>%
    dplyr::filter(x >= -129.68,
                  x < -128.66,
                  y >= 52.8,
                  y < 53.55) %>%
    dplyr::select(vid,
                  type,
                  speed,
                  length,
                  width,
                  draft,
                  datetime,
                  x,
                  y)
  ais %>% head
  products <- rbind(products, ais)
  message(i)
}

head(products)
nrow(products)

# Add escort tugs ==============================================================

tug_length <- 35
4/27 # width:length ratio from AIS 2019 tugs
(tug_width <- (4 / 27)* 35)
tug_draft <- 3.1 # mean draft from AIS 2019 tugs

tug_heels <- heels
tug_heels$type <- 'Cedar LNG tug in-heel'
tug_heels$length <- tug_length
tug_heels$width <- tug_width
tug_heels$draft <- tug_draft

tug_product <- products
tug_product$type <- 'Cedar LNG tug in-product'
tug_product$length <- tug_length
tug_product$width <- tug_width
tug_product$draft <- tug_draft

# Combine in-heel and products
ais <- rbind(heels, products, tug_heels, tug_product)

ais$type %>% table

################################################################################
################################################################################
# Complete processing

# Interpolate vessel grid
vgrid <- vessel_grid(grid_kfs, ais, verbose=TRUE)

# Join channel to each grid number
nrow(vgrid)
head(vgrid)

head(grid_kfs)
grid_join <- grid_kfs %>% select(grid_id = id, channel = block)
head(grid_join)
vgrid2 <- left_join(vgrid, grid_join, by='grid_id')
head(vgrid2)
vgrid2$channel %>% table

# Check distances
vgrid2 %>% group_by(channel, type) %>% summarize(kms = sum(km))

# Add sun angles
vgrid2$sun <- vessel_sun_angle(vgrid2, verbose=TRUE)
vgrid2$diel <- 'day'
vgrid2$diel[vgrid2$sun < -12] <- 'night' # nautical dawn/dusk
vgrid2$diel %>% table

# Save output
cedar_lng <- vgrid2
head(cedar_lng)
saveRDS(cedar_lng, file='tests/vessels_cedar_lng_8_14knots.RData')

