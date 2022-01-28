################################################################################
# p(whale)
################################################################################

library(dplyr)

getwd()
lta <- readRDS('tests/lta/lta-data.rds')
head(lta)

eff <- lta$eff %>% dplyr::mutate(day = gsub('-','',day))
head(eff)

lta$siw13 %>% head
sit13 <- lta$siw13 %>%
  dplyr::filter(eff==1) %>%
  dplyr::select(sp = spp,
                distance = dist.track,
                size = best,
                datetime = date,
                block,
                bft,
                lon = X,
                lat = Y) %>%
  dplyr::mutate(year = 2013) %>%
  dplyr::mutate(day = gsub('-','',substr(datetime,1,10)))
head(sit13)
# Need to correct X/Y

lta$siw14 %>% head
sit14 <- lta$siw14 %>%
  dplyr::filter(eff==1) %>%
  dplyr::select(sp,
                distance = dist.track,
                size = grp.best,
                datetime = date,
                block,
                bft,
                lon = X,
                lat = Y) %>%
  dplyr::mutate(year = 2014) %>%
  dplyr::mutate(day = gsub('-','',substr(datetime,1,10)))
head(sit14)

lta$siw15 %>% head
sit15 <- lta$siw15 %>%
  dplyr::filter(eff==1) %>%
  dplyr::select(sp,
                distance = dist.track,
                size = grp.best,
                datetime = date,
                block,
                bft,
                lon = X,
                lat = Y) %>%
  dplyr::mutate(year = 2015) %>%
  dplyr::mutate(day = gsub('-','',substr(datetime,1,10)))
head(sit15)

sits <- rbind(sit13, sit14, sit15)
nrow(sits)

sits
sits$distance %>% hist

################################################################################
# Format region labels

eff %>% group_by(block) %>% summarize(Region.Label = unique(Region.Label))
sits$Region.Label <- NA
sits$Region.Label[sits$block %in% c('CAM','CAA')] <- 1
sits$Region.Label[sits$block %in% c('CMP')] <- 3
sits$Region.Label[sits$block %in% c('EST')] <- 2
sits$Region.Label[sits$block %in% c('SQU')] <- 4
sits$Region.Label[sits$block %in% c('VER','WHA','WRI','MCK')] <- 5
sits$Region.Label %>% table

################################################################################
# Associate with physiographic features

library(readr)
seafloor <- read_csv('tests/lta/depthframe.csv')
head(seafloor)
nrow(seafloor)

sits %>% head

get_seafloor <- function(lon,lat,seafloor){
  #lon <- -129.4380
  #lat <- 53.17300
  z <- zmin <- zmax <- zsd <- NA

  # get seafloors within a km
  seas <- seafloor %>% dplyr::filter(x >= lon - 0.009009,
                                     x <= lon + 0.009009,
                                     y >= lat - 0.009009,
                                     y <= lat + 0.009009)
  # Of these, find range & sd
  (zmin <- abs(max(seas$layer)))
  (zmax <- abs(min(seas$layer)))
  (zsd <- abs(sd(seas$layer)))

  # Of these, find closest depth reading to lat/long
  seas$km <- apply(seas,1,function(seasi){
    swfscMisc::distance(lat1 = lat,
                        lon1 = lon,
                        lat2 = seasi[2],
                        lon2 = seasi[1],
                        units='km')
  })
  (km_min <- min(seas$km))
  matchi <- NULL
  if(km_min < .25){matchi <- which.min(seas$km)}
  if(!is.null(matchi)){z <- abs(seas$layer[matchi])}
  zf <- data.frame(z, zmin, zmax, zsd)
  return(zf)
}

i=1
newsits <- data.frame()
for(i in 1:nrow(sits)){
  (siti <- sits[i,])
  (z <- get_seafloor(lon = siti$lon, lat = siti$lat, seafloor))
  z$zrange <- z$zmax - z$zmin
  siti <- data.frame(siti,z)
  newsits <- rbind(newsits, siti)
}
newsits


################################################################################
# Format for distance

eff %>% head
newsits %>% head

# Table with sightings
(data_table <-
    newsits %>%
    dplyr::transmute(distance = distance,
                     size = size,
                     Sample.Label = day,
                     Region.Label = Region.Label,
                     year = year,
                     bft = bft,
                     z = z,
                     zrange = zrange,
                     zsd = zsd) %>%
    dplyr::mutate(object = 1:dplyr::n()))

# Table linking sightings to segments
(obs_table <- data_table %>% dplyr::select(object, Region.Label, Sample.Label))

# Sample (segments) table
(sample_table <- eff %>% dplyr::transmute(Sample.Label = day,
                                          Region.Label = Region.Label,
                                          Effort = km,
                                          year = substr(day,1,4)))

################################################################################
# Fit detection functions

data_table %>% head
obs_table %>% head
sample_table %>% head

# Load the prediction grid (which is the grid cells where vessels occurred)
load('tests/vessels_sim.RData')
vessels_sim %>% head


# 10%  truncation distance is ~2.0km
data_table$distance
quantile(data_table$distance, 0.91, na.rm=TRUE)

dso1 <- Distance::ds(data = data_table, truncation = 2.0,
                    formula= ~1,
                    key = 'hn', quiet=FALSE)

dso2 <- Distance::ds(data = data_table, truncation = 2.0,
                     formula= ~1 + bft,
                     adjustment = NULL, key = 'hn', quiet=FALSE)

dso3 <- Distance::ds(data = data_table, truncation = 2.0,
                     formula= ~1 + factor(year),
                     adjustment = NULL, key = 'hn', quiet=FALSE)

Distance::summarize_ds_models(dso1, dso2, dso3)

plot(dso1)


################################################################################
# bring in detailed effort
load('tests/lta/effort13.RData')
load('tests/lta/effort14.RData')
load('tests/lta/effort15.RData')

head(eff)
eff$day %>% table %>% table

# Get total surveyed each day
(days <- eff %>%
    dplyr::group_by(day) %>%
    dplyr::summarize(Effort = sum(km),
                     blocks = paste(block,collapse='-')) %>%
    dplyr::rename(Sample.Label = day))

# Get
head(data_table)

# May only be able to use 2014 - 2015 effort
effort14 %>% head
effort15 %>% head

# Next step: create segment.data from 14 and 15
# associate segment.label with each day of effort
# Get z for each effort point

# Get z for each vsumm grid cell

# Get block (SQU, CMP, CAA, OTHER) for each grid cell and each effort point

# Then fit DSM

library(dsm)
dsm.xy <- dsm(count~s(x,y),
              dso1,
              sample_table,
              data_table,
              method="REML")





source('tests/parameters.R')

# Create p(whale) distribution
# Draw 10,000 random draws from each grid cell according to its mean / CV
# Pool through draws to create the p(whale) distribution
pwhale <- truncnorm::rtruncnorm(n=10000,
                                mean = vsumm$d_mean,
                                sd = vsumm$d_cv * vsumm$d_mean,
                                a=0)
hist(pwhale, breaks=20)
length(pwhale)





################################################################################




