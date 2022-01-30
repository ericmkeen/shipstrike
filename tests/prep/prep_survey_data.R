# Prep transect data


################################################################################
################################################################################
# Prep data
################################################################################
################################################################################

################################################################################
# Bring in detailed effort
load('tests/lta/effort13.RData')
load('tests/lta/effort14.RData')
load('tests/lta/effort15.RData')

# Bring in effort summary
head(eff)
eff$day %>% table %>% table

# Use summary to get total surveyed each day (to help fill in 2013 data)
(days <- eff %>%
    dplyr::group_by(day) %>%
    dplyr::summarize(Effort = sum(km),
                     blocks = paste(block,collapse='-')) %>%
    dplyr::rename(Sample.Label = day))

days %>% as.data.frame %>% head(20)

################################################################################
# Format each year

# 2013 effort ==================================================================
effort13 %>% head
eff13 <-
  effort13 %>%
  dplyr::transmute(day = gsub('-','',substr(date,1,10)),
                   datetime = date,
                   lat = lat,
                   lon = long,
                   block = block,
                   bft = NA)
eff13 %>% head
eff13 <- eff13 %>% dplyr::filter(!is.na(day))
eff13$block[eff13$day == '20130706'] <- 'VER'
eff13$block[eff13$day == '20130707'] <- 'MCK'
eff13$block[eff13$day == '20130708'] <- 'WRI'
eff13$block[eff13$day == '20130713'] <- 'WRI'
eff13$block[eff13$day == '20130714'] <- 'SQU'
eff13$block[eff13$day == '20130715'] <- 'CMP'
eff13$block[eff13$day == '20130716'] <- 'CMP'
eff13$block[eff13$day == '20130717'] <- 'CAA'
eff13$block[eff13$day == '20130718'] <- 'WHA'
eff13$block[eff13$day == '20130719'] <- 'CMP'
eff13$block[eff13$day == '20130726'] <- 'CMP'
eff13$block[eff13$day == '20130818'] <- 'WHA'
eff13$block[eff13$day == '20130819'] <- 'VER'
eff13$block[eff13$day == '20130820'] <- 'MCK'
eff13$block[eff13$day == '20130822'] <- 'SQU'
eff13$block[eff13$day == '20130824'] <- 'VER'
eff13$block[eff13$day == '20130827'] <- 'WRI'
eff13$block[eff13$day == '20130828'] <- 'SQU'
eff13$block[eff13$day == '20130829'] <- 'CMP'
eff13$block[eff13$day == '20130904'] <- 'CAA'
eff13$block %>% table(useNA='ifany')

# 2014 =========================================================================

effort14 %>% head
eff14 <-
  effort14 %>%
  dplyr::transmute(day = gsub('-','',substr(date,1,10)),
                   datetime = date,
                   lat = lat,
                   lon = long,
                   block = block,
                   bft = bft)

# 2015 =========================================================================

eff15 <-
  effort15 %>%
  dplyr::transmute(day = gsub('-','',substr(date,1,10)),
                   datetime = date,
                   lat = lat,
                   lon = long,
                   block = block,
                   bft = bft)
das <- rbind(eff13, eff14, eff15)
das %>% head

neweff <- data.frame()
days <- unique(das$day)
for(i in 1:length(days)){
  print(i)
  dass <- das %>% dplyr::filter(day == days[i])
  nrow(dass)
  pos <- data.frame(lat1 = dass$lon[1:(nrow(dass)-1)],
                    lon1 = dass$lat[1:(nrow(dass)-1)],
                    lat2 = dass$lon[2:(nrow(dass))],
                    lon2 = dass$lat[2:(nrow(dass))])
  head(pos)
  nrow(pos)
  km <- apply(pos,1,function(x){
    d <- NA
    if(all(!is.na(x))){
      d <- swfscDAS::distance_greatcircle(x[1], x[2], x[3], x[4])
    }
    return(d)
  })
  km <- c(km, 0)
  dass$km <- km[1:nrow(dass)]
  neweff <- rbind(neweff, dass)
}
nrow(neweff)

################################################################################
# Assign segments

seg_km <- 5 # 5 km segments
seg_counter <- 1
segment_summary <- data.frame()
segments <- data.frame()
(days <- unique(neweff$day))
di=1
for(di in 1:length(days)){
  #print(di)
  effday <- neweff %>% dplyr::filter(day == days[di])
  seg_km_tot <- 0
  i=1
  for(i in 1:nrow(effday)){
    (effi <- effday[i,])
    seg_counter
    seg_km_tot
    effi$seg_id <- seg_counter
    segments <- rbind(segments, effi)
    if(!is.na(effi$km)){
      seg_km_tot <- seg_km_tot + effi$km
      if(seg_km_tot >= seg_km | i == nrow(effday)){
        summi <- data.frame(seg_id = seg_counter, km = seg_km_tot)
        segment_summary <- rbind(segment_summary, summi)
        seg_counter <- seg_counter + 1
        seg_km_tot <- 0
      }
    }
  }
  message(days[di],' :: total segments to date = ',seg_counter,' ...')
}

################################################################################
# Summarize segments
# As you do, find seafloor attributes

library(bangarang)
par(mfrow=c(1,1))
plotKFS()

segment_table <- data.frame()
segids <- segments$seg_id %>% unique
i=1
for(i in 1:length(segids)){
  (segidi <- segids[i])
  segi <- segments %>% dplyr::filter(seg_id == segidi)
  segi

  xmid <- segi$lon %>% mean(na.rm=TRUE)
  ymid <- segi$lat %>% mean(na.rm=TRUE)
  dtmid <- segi$datetime[1]
  segtabi <- data.frame(Sample.Label = segi$seg_id[1],
                        Effort = sum(segi$km, na.rm=TRUE),
                        x = xmid,
                        y = ymid,
                        datetime = dtmid,
                        year = substr(segi$day[1],1,4),
                        day = segi$day[1],
                        block = segi$block[1])
  segment_table <- rbind(segment_table, segtabi)

  # plot it
  points(x=xmid, y=ymid, pch=16, cex=1, col=adjustcolor('darkblue',alpha=.4))
  #lines(x=segi$lon, y=segi$lat, col=adjustcolor('darkblue',alpha=.2),lwd=1.9)
}

segment_table %>% head
nrow(segment_table)
hist(segment_table$Effort, breaks=seq(0,150,by=1))



################################################################################
# Format sightings

# 2013 ============================================

siw <- read.csv('tests/lta/2013 sits.csv', stringsAsFactors=FALSE)
head(siw)
siw$spp %>% table

# Calcualte trackline distance
trackdist <- vector() ; i=1
for(i in 1:nrow(siw)){
  c <- as.numeric(siw$dist[i]) ; c
  if(is.na(c)==TRUE){
    trackdist[i] <- NA
  }else{
    theta <- as.numeric(siw$bear[i]) - as.numeric(siw$hdg[i]) ; theta
    if(is.na(theta)==TRUE){
      trackdist[i] <- NA
    }else{
      if(theta < 0){theta <- 360-theta}
      radians <- theta * (pi/180)
      trackdist[i] <- c*sin(theta)
    }
  }
}
siw$dist.track <- abs(trackdist) ; siw

sit13 <-
  siw %>%
  dplyr::filter(eff == 1,
                spp %in% c('BW','FW','HW','DP')) %>%
  dplyr::mutate(day = gsub('-','',substr(date,1,10))) %>%
  dplyr::select(day,
                datetime = date,
                x = X,
                y = Y,
                distance = dist.track,
                size = best,
                spp, block, bft)
sit13 %>% head

# 2014 ============================================

siw <- read.csv('tests/lta/2014 sits.csv', stringsAsFactors=FALSE)
head(siw)
siw$sp %>% table

sit14 <-
  siw %>%
  dplyr::filter(eff == 1,
                sp %in% c('BW','FW','HW','DP')) %>%
  dplyr::mutate(day = gsub('-','',substr(date,1,10))) %>%
  dplyr::select(day,
                datetime = date,
                x = X,
                y = Y,
                distance = dist.track,
                size = grp.best,
                spp = sp, block, bft)
sit14 %>% head

# 2015 ============================================


siw <- read.csv('tests/lta/2015 sits.csv', stringsAsFactors=FALSE)
head(siw)
siw$sp %>% table

sit15 <-
  siw %>%
  dplyr::filter(eff == 1,
                sp %in% c('BW','FW','HW','DP')) %>%
  dplyr::mutate(day = gsub('-','',substr(date,1,10))) %>%
  dplyr::select(day,
                datetime = date,
                x = X,
                y = Y,
                distance = dist.track,
                size = grp.best,
                spp = sp, block, bft)
sit15 %>% head

# Combine ============================================

sits <- rbind(sit13, sit14, sit15)
head(sits)

################################################################################
# Get segment ID for each sighting

head(sits)
nrow(sits)
head(segment_table)

i=552
seg_ids <- c()
for(i in 1:nrow(sits)){
  print(i)
  (siti <- sits[i,])
  (segday <- segment_table %>% dplyr::filter(day == siti$day))
  seg_ids[i] <- NA
  if(nrow(segday)>0){
    (diffs <- difftime(siti$datetime, segday$datetime, units='secs'))
    (mindiff <- min(abs(diffs),na.rm=TRUE))
    if(mindiff < (3600*3)){
      matchi <- which.min(abs(difftime(siti$datetime, segday$datetime, units='secs')))
      seg_ids[i] <- segday$Sample.Label[matchi]
    }
  }
}
seg_ids
sits$seg_id <- seg_ids
sits %>% head

################################################################################
# Seafloor finding function

library(readr)
seafloor <- readr::read_csv('tests/lta/depthframe.csv')

# Get z for each effort point ==================================================

i=1
newsegments <- data.frame()
for(i in 1:nrow(segment_table)){
  print(i)
  (segi <- segment_table[i,])
  (z <- get_seafloor(lon = segi$x, lat = segi$y, seafloor))
  z$zrange <- z$zmax - z$zmin
  segi <- data.frame(segi,z)
  newsegments <- rbind(newsegments, segi)
}
newsegments

# Get z for each sighting  =====================================================

i=1
newsits <- data.frame()
for(i in 1:nrow(sits)){
  print(i)
  (siti <- sits[i,])
  (z <- get_seafloor(lon = siti$x, lat = siti$y, seafloor))
  z$zrange <- z$zmax - z$zmin
  siti <- data.frame(siti,z)
  newsits <- rbind(newsits, siti)
}
newsits

################################################################################
# Clean up data & save

segments <-
  newsegments %>%
  dplyr::filter(Effort < 30)
nrow(segments)

sightings <-
  newsits %>%
  dplyr::filter(seg_id %in% segments$Sample.Label)
nrow(sightings)

write.csv(segments, file='tests/lta/bangarang_segments.csv',quote=FALSE,row.names=FALSE)
write.csv(sightings, file='tests/lta/bangarang_sightings.csv',quote=FALSE,row.names=FALSE)
