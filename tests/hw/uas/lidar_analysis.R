library(dplyr)
library(stringr)
library(lubridate)
library(gsheet)
library(swfscMisc)

# Get GoogleSheet of launch metadata
url <- 'https://docs.google.com/spreadsheets/d/1iotGV7Hwe84piCNZiYArgtLelOL2jBk9ddh1NHTyMu4/edit?usp=sharing'
df <- gsheet::gsheet2tbl(url)
# Rename columns
df %>% names
names(df) <- c('video','lidar','launch','prev1','prev2','prev3',
               'start_file','end_file','comment')
df %>% head

# Get lidar files & snapshot files from Google Drive
# library(googledrive)
# use emkeen@sewanee.edu account
#drives <- googledrive::drive_find(pattern='HW-2019-lidar', type = 'folder', n_max = 30)
#(lidar_files <- googledrive::drive_ls(drives$id[1]))
#(snapshot_files <- googledrive::drive_ls(drives$id[2])$name)

# Lidar files
lidar_path <- 'LIDAR/'
(lf <- list.files(lidar_path))
(lidar_files <- paste0(lidar_path, lf))

################################################################################
# Loop through unique videos

(video_files <- df$video %>% unique)
mr <- data.frame()
i=1
for(i in 1:nrow(df)){
  print(i)

  (dfi <- df[i,])

  # Launch info
  (launch_video <- str_split(dfi$video,'_')[[1]])
  (launch_date <- launch_video[1])
  (launch_file <- launch_video[3] %>% as.numeric)
  (launch_time <- dfi$launch)
  (li_hr <- str_split(launch_time,':')[[1]][1])
  (li_mm <- str_split(launch_time,':')[[1]][2])
  (li_ss <- str_split(launch_time,':')[[1]][3])
  (li_secs <- (as.numeric(li_hr)*3600) + (as.numeric(li_mm)*60) + as.numeric(li_ss))
  (li_group <- str_split(dfi$video,'Group')[[1]])
  if(length(li_group)>1){li_group <- li_group[length(li_group)]}else{li_group <- 1}
  (li_group <- substr(as.character(li_group),1,1) %>% as.numeric)

  # Start of focal follow
  (starti <- dfi$start_file)
  (dash_split <- str_split(starti,'-')[[1]])
  (start_video <- dash_split[1])
  (start_file <- str_split(start_video,'DJI_')[[1]][2] %>% substr(1,4) %>% as.numeric)
  (bi <- dash_split[2])
  (bi_hr <- str_split(bi,'_')[[1]][1])
  (bi_mm <- str_split(bi,'_')[[1]][2])
  (bi_ss <- str_split(bi,'_')[[1]][3])
  (bi_secs <-
      ((start_file - launch_file)*301) + # this is in case the video spilled into a subsequent file
      (as.numeric(bi_hr)*3600) + (as.numeric(bi_mm)*60) + as.numeric(bi_ss))

  # End of focal follow
  (endi <- dfi$end_file)
  (dash_split <- str_split(endi,'-')[[1]])
  (end_video <- dash_split[1])
  (end_file <- str_split(end_video,'DJI_')[[1]][2] %>% substr(1,4) %>% as.numeric)
  (ei <- dash_split[2])
  (ei_hr <- str_split(ei,'_')[[1]][1])
  (ei_mm <- str_split(ei,'_')[[1]][2])
  (ei_ss <- str_split(ei,'_')[[1]][3])
  (ei_secs <-
      ((end_file - launch_file)*301) + # this is in case the video spilled into a subsequent file
      (as.numeric(ei_hr)*3600) + (as.numeric(ei_mm)*60) + as.numeric(ei_ss))

  mri <- data.frame(launch_date = lubridate::ymd(launch_date),
                    launch_video = dfi$video,
                    lidar_file = dfi$lidar,
                    launch_group = li_group,
                    launch_secs = li_secs,
                    begin_secs_from_launch= (bi_secs - li_secs),
                    end_secs_from_launch = (ei_secs - li_secs)
  )
  mri$duration <- mri$end_secs_from_launch - mri$begin_secs_from_launch
  #print(mri)

  mr <- rbind(mr, mri)

}

mr

################################################################################
# Process LIDAR files

lidar_files
(lidar_files <- lidar_files[ ! grepl('Icon',lidar_files) ])
lidar_data <- list()
mrl <- data.frame()
i=8
for(i in 1:length(lidar_files)){
  (lfi <- lidar_files[i])
  #drive_download(lfi$id, path='lidar_temp.csv', overwrite=TRUE)
  #lidari <- read.csv('lidar_temp.csv', sep='\t', skip=2)
  lidari <- read.csv(lfi, sep='\t', skip=2)
  lidari %>% head

  lidari$dt <-
    paste0(lidari$X.gmt_date,' ', lidari$gmt_time) %>%
    lubridate::ymd_hms(tz='UTC') %>%
    lubridate::with_tz(tzone='Canada/Pacific')

  lidari$cm <- lidari$laser_altitude_cm
  lidari$cm %>% head(100)
  (lidari$cm_diff <- c(0, lidari$cm[2:nrow(lidari)] - lidari$cm[1:(nrow(lidari)-1)]))
  lidari$cm_diff
  (launch_i <- which(lidari$cm_diff > 100)[1])
  verdicts <- sapply(1:(nrow(lidari)-3), function(x){all(c(lidari$cm_diff[x] > 50,
                                               lidari$cm_diff[x:(x+2)] > 20))})
  launch_i <- which(verdicts)[1]
  if(i == 8){launch_i = 11}

  #launch_i <- 120
  par(mfrow=c(2,1))
  par(mar=c(4.2,4.2,3,.5))
  plot(lidari$cm,
       ylim=c(0, 5000), pch=16, col=adjustcolor('black',alpha.f=.4),
       main=paste0('File ',i,' :: ', lfi))
  abline(v=launch_i, lwd=2, col='firebrick')
  par(mar=c(4.2,4.2,.5,.5))
  plot(lidari$cm_diff,
       pch=16, col=adjustcolor('black',alpha.f=.4),
       main=NULL)
  abline(h=0, col='cadetblue3', lty=3)
  abline(v=launch_i, lwd=2, col='firebrick')
  par(mfrow=c(1,1))

  lidar_data[[length(lidar_data)+1]] <- lidari

  mrli <- data.frame(list_index=i,
                     launch_dt = lidari$dt[launch_i],
                     launch_index = launch_i,
                     launch_date = lubridate::ymd(lidari$X.gmt_date[launch_i]),
                     launch_time = lidari$gmt_time[launch_i],
                     drive_fn = lfi)
  #mrli$launch_dt <- paste0(mrli$launch_date,' ', mrli$launch_time) %>% lubridate::ymd_hms(tz='UTC') %>% lubridate::with_tz(tzone='Canada/Pacific')
  mrli$launch_date <- mrli$launch_dt %>% lubridate::with_tz(tzone='Canada/Pacific') %>% lubridate::as_date()
  mrli
  mrl <- rbind(mrl, mrli)

  #readline(prompt="Press [enter] to continue")
  Sys.sleep(2)

}

mrl
lidar_data

################################################################################
# Now sync snapshot log to lidar log

# Join according to associated lidar file
(mr <- mr %>% arrange(launch_date))
(mrl <- mrl %>% arrange(launch_dt))
i=2
MR <- data.frame()
for(i in 1:nrow(mr)){
  (mri <- mr[i,])
  (lidari <- which(gsub('LIDAR/', '', mrl$drive_fn) == mri$lidar))
  if(length(lidari)>0){
    mrli <- mrl[lidari,]
    mri <- data.frame(mri, mrli)
    MR <- rbind(MR, mri)
  }
}
MR

# Calculate LIDAR begin and end times
(MR$begin_dt <- MR$launch_dt + lubridate::seconds(MR$begin_secs_from_launch))
(MR$end_dt <- MR$launch_dt + lubridate::seconds(MR$end_secs_from_launch))

# Now find closest lat/long coordinates for those begin and end times
MR$xb <- MR$yb <- MR$offsetb <- MR$xe <- MR$ye <- MR$offsete <- NA
i=1
for(i in 1:nrow(MR)){
  (mri <- MR[i,])
  (lidari <- lidar_data[[mri$list_index]]) %>% head

  # Begin lat/long/offset
  (diffs <- difftime(mri$begin_dt, lidari$dt, units='secs'))
  (mini <- which.min(abs(diffs)))
  MR$offsetb[i] <- diffs[mini]
  MR$xb[i] <- lidari$longitude[mini]
  MR$yb[i] <- lidari$latitude[mini]

  # End lat/long/offset
  (diffs <- difftime(mri$end_dt, lidari$dt, units='secs'))
  (mini <- which.min(abs(diffs)))
  MR$offsete[i] <- diffs[mini]
  MR$xe[i] <- lidari$longitude[mini]
  MR$ye[i] <- lidari$latitude[mini]
}

MR

(MR$km <- swfscMisc::distance(lat1 = MR$yb, lon1 = MR$xb,
                  lat2 = MR$ye, lon2 = MR$xb,
                  units = 'nm') * 1.852)
(MR$mps <- (MR$km*1000)/MR$duration)

# Group by flight, so that each flight is treated as a single sample
# For each flight, only keep samples of more than 20 seconds duration
# Average smaples from each flight together

MR <-
  MR %>%
  filter(duration > 30) %>%
  group_by(launch_date, launch_video) %>%
  summarize(n=n(),
            duration_mean = mean(duration),
            duration_sum = sum(duration),
            km_mean = mean(km),
            km_sum = sum(km),
            mps = mean(mps)) %>%
  arrange(launch_date)

# Results
MR %>% nrow
MR$mps %>% hist()
MR$mps %>% mean(na.rm=TRUE)
MR$mps %>% sd(na.rm=TRUE)
MR$mps %>% min(na.rm=TRUE)
MR$mps %>% max(na.rm=TRUE)

saveRDS(MR, file='hw_lidar_results.RData')
