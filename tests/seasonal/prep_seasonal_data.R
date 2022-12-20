################################################################################
################################################################################
# Prep seasonal data
################################################################################
################################################################################

library(dplyr)
library(lubridate)
library(ggplot2)

# Stage final dataset
MR <- data.frame()

################################################################################
# 2017
################################################################################

#Bring in data
workdir <- "tests/seasonal/fin/2017/"
lf <- list.files(workdir) ; lf
mm <- read.csv(paste0(workdir,"mm.csv"),stringsAsFactors=FALSE) ; head(mm)
scans <- read.csv(paste0(workdir,"scans.csv"),stringsAsFactors=FALSE) ; head(scans)
seas <- read.csv(paste0(workdir,"sea.csv"),stringsAsFactors=FALSE) ; head(sea)

# Scans - determine good-visibility scans
vis_ok <- c()
i=1
for(i in 1:nrow(scans)){
  (scani <- scans[i,])
  (seasi <- seas %>% dplyr::filter(DATE == scani$DATE, HOUR == scani$HOUR, Vis.km >= 10))
  vis_ok[i] <- ifelse(nrow(seasi)>0, TRUE, FALSE)
}
vis_ok %>% table
scans$vis_ok <- vis_ok

# Marine mammals - determine good-visibility scans
vis_ok <- c()
i=1
for(i in 1:nrow(mm)){
  (mmi <- mm[i,])
  (seasi <- seas %>% dplyr::filter(DATE == mmi$scan.day, HOUR == mmi$scan.hour, Vis.km >= 10))
  vis_ok[i] <- ifelse(nrow(seasi)>0, TRUE, FALSE)
}
vis_ok %>% table
mm$vis_ok <- vis_ok

scans <- scans %>% dplyr::filter(vis_ok == TRUE)
mm <- mm %>% dplyr::filter(vis_ok == TRUE)

# Format other columns
mm$Best <- as.numeric(as.character(mm$Best))
(mm$j <- mm$Date %>%
    lubridate::as_date() %>%
    lubridate::yday())
(scans$j <-
  paste0(substr(scans$DATE,1,4),'-',
         substr(scans$DATE,5,6),'-',
         substr(scans$DATE,7,8)) %>%
  lubridate::as_date() %>%
  lubridate::yday())

mm$wk <- floor(mm$j / 14)
scans$wk <- floor(scans$j / 14)

mm$wk %>% range
scans$wk %>% range

head(mm)
head(scans)
unique(mm$Species)

mr <- data.frame()
i=11
for(i in 1:26){
  mmi <- mm[mm$wk==i,] ; head(mmi)
  sci <- scans[scans$wk==i,] ; head(sci)
  ti <- i*14
  if(nrow(sci)>0){
    mi <- sum(sci$MINUTES,na.rm=TRUE) ; mi
    fi <- sum(mmi$Best[grep("Fin",mmi$Species)],na.rm=TRUE) ; fi
    hi <- sum(mmi$Best[grep("Hump",mmi$Species)],na.rm=TRUE) ; hi
  }else{
    mi <- 0 ; fi <- hi <- NA
  }
  mri <- data.frame(t=ti,m=mi,fw=fi,hw=hi)  ; mri
  mr <- rbind(mr,mri)
}
mr

# Limit annual range
mr <- mr[mr$t >= 100 & mr$t <= 300,]

# Scale to a rate
mr$fw.r <- mr$fw / mr$m
mr$hw.r <- mr$hw / mr$m

# Scale to move y vals to (0,1)
mr$fw.s <- mr$fw.r / (max(mr$fw.r,na.rm=TRUE)) ; mr$fw.s
mr$hw.s <- mr$hw.r / (max(mr$hw.r,na.rm=TRUE)) ; mr$hw.s

# Add year and platform
mr$year <- 2017
mr$platform <- 'Fin Island'
MR <- rbind(MR, mr)

par(mfrow=c(5,2)) ; par(mar=c(4.2,4.2,3,1))
plot(mr$hw.s~mr$t,type="b",ylim=c(0,1),main="Humpback 2017",pch=16,xlab="Day of year",ylab="Encounter rate")
abline(h=1,lty=3)
plot(mr$fw.s~mr$t,type="b",ylim=c(0,1),main="Fin 2017",pch=16,xlab="Day of year",ylab="Encounter rate")
abline(h=1,lty=3)

################################################################################
# 2018
################################################################################

#Bring in data
workdir <- "tests/seasonal/fin/2018/"
lf <- list.files(workdir) ; lf
mm <- read.csv(paste0(workdir,"mm.csv"),stringsAsFactors=FALSE) ; head(mm)
scans <- read.csv(paste0(workdir,"scans.csv"),stringsAsFactors=FALSE) ; head(scans)
seas <- read.csv(paste0(workdir,"sea.csv"),stringsAsFactors=FALSE) ; head(sea)

# Scans - determine good-visibility scans
vis_ok <- c()
i=1
for(i in 1:nrow(scans)){
  (scani <- scans[i,])
  (seasi <- seas %>% dplyr::filter(DATE == scani$DATE, HOUR == scani$HOUR, Vis.km >= 10))
  vis_ok[i] <- ifelse(nrow(seasi)>0, TRUE, FALSE)
}
vis_ok %>% table
scans$vis_ok <- vis_ok

# Marine mammals - determine good-visibility scans
vis_ok <- c()
i=1
for(i in 1:nrow(mm)){
  (mmi <- mm[i,])
  (seasi <- seas %>% dplyr::filter(DATE == mmi$scan.day, HOUR == mmi$scan.hour, Vis.km >= 10))
  vis_ok[i] <- ifelse(nrow(seasi)>0, TRUE, FALSE)
}
vis_ok %>% table
mm$vis_ok <- vis_ok

scans <- scans %>% dplyr::filter(vis_ok == TRUE)
mm <- mm %>% dplyr::filter(vis_ok == TRUE)

# Format other columns
(mm$Best <- as.numeric(as.character(mm$Best)))
(mm$j <- mm$Date %>%
    lubridate::as_date() %>%
    lubridate::yday())
(scans$j <-
    paste0(substr(scans$DATE,1,4),'-',
           substr(scans$DATE,5,6),'-',
           substr(scans$DATE,7,8)) %>%
    lubridate::as_date() %>%
    lubridate::yday())

mm$wk <- floor(mm$j / 14)
scans$wk <- floor(scans$j / 14)

mm$wk %>% range
scans$wk %>% range

head(mm)
head(scans)
unique(mm$Species)

mr <- data.frame()
i=11
for(i in 1:26){
  mmi <- mm[mm$wk==i,] ; head(mmi)
  sci <- scans[scans$wk==i,] ; head(sci)
  ti <- i*14
  if(nrow(sci)>0){
    mi <- sum(sci$MINUTES,na.rm=TRUE) ; mi
    fi <- sum(mmi$Best[grep("Fin",mmi$Species)],na.rm=TRUE) ; fi
    hi <- sum(mmi$Best[grep("Hump",mmi$Species)],na.rm=TRUE) ; hi
  }else{
    mi <- 0 ; fi <- hi <- NA
  }
  mri <- data.frame(t=ti,m=mi,fw=fi,hw=hi)  ; mri
  mr <- rbind(mr,mri)
}
mr

# Limit annual range
mr <- mr[mr$t >= 100 & mr$t <= 300,]

# Scale to a rate
mr$fw.r <- mr$fw / mr$m
mr$hw.r <- mr$hw / mr$m

# Scale to move y vals to (0,1)
mr$fw.s <- mr$fw.r / (max(mr$fw.r,na.rm=TRUE)) ; mr$fw.s
mr$hw.s <- mr$hw.r / (max(mr$hw.r,na.rm=TRUE)) ; mr$hw.s

# Add year and platform
mr$year <- 2018
mr$platform <- 'Fin Island'
MR <- rbind(MR, mr)
head(mr)

par(mar=c(4.2,4.2,3,1))
plot(mr$hw.s~mr$t,type="b",ylim=c(0,1),main="Humpback 2018",pch=16,xlab="Day of year",ylab="Encounter rate")
abline(h=1,lty=3)
plot(mr$fw.s~mr$t,type="b",ylim=c(0,1),main="Fin 2018",pch=16,xlab="Day of year",ylab="Encounter rate")
abline(h=1,lty=3)

################################################################################
# 2019
################################################################################

#Bring in data
workdir <- "tests/seasonal/fin/2019/"
lf <- list.files(workdir) ; lf
mm <- read.csv(paste0(workdir,"mm.csv"),stringsAsFactors=FALSE) ; head(mm)
scans <- read.csv(paste0(workdir,"scans.csv"),stringsAsFactors=FALSE) ; head(scans)
seas <- read.csv(paste0(workdir,"sea.csv"),stringsAsFactors=FALSE) ; head(sea)

# Scans - determine good-visibility scans
vis_ok <- c()
i=1
for(i in 1:nrow(scans)){
  (scani <- scans[i,])
  (seasi <- seas %>% dplyr::filter(DATE == scani$DATE, HOUR == scani$HOUR, Vis.km >= 10))
  vis_ok[i] <- ifelse(nrow(seasi)>0, TRUE, FALSE)
}
vis_ok %>% table
scans$vis_ok <- vis_ok

# Marine mammals - determine good-visibility scans
vis_ok <- c()
i=1
for(i in 1:nrow(mm)){
  (mmi <- mm[i,])
  (seasi <- seas %>% dplyr::filter(DATE == mmi$scan.day, HOUR == mmi$scan.hour, Vis.km >= 10))
  vis_ok[i] <- ifelse(nrow(seasi)>0, TRUE, FALSE)
}
vis_ok %>% table
mm$vis_ok <- vis_ok

scans <- scans %>% dplyr::filter(vis_ok == TRUE)
mm <- mm %>% dplyr::filter(vis_ok == TRUE)

# Format other columns
(mm$Best <- as.numeric(as.character(mm$Best)))
(mm$j <- mm$Date %>%
    lubridate::as_date() %>%
    lubridate::yday())
(scans$j <-
    paste0(substr(scans$DATE,1,4),'-',
           substr(scans$DATE,5,6),'-',
           substr(scans$DATE,7,8)) %>%
    lubridate::as_date() %>%
    lubridate::yday())

mm$wk <- floor(mm$j / 14)
scans$wk <- floor(scans$j / 14)

mm$wk %>% range
scans$wk %>% range

head(mm)
head(scans)
unique(mm$Species)

mr <- data.frame()
i=11
for(i in 1:26){
  mmi <- mm[mm$wk==i,] ; head(mmi)
  sci <- scans[scans$wk==i,] ; head(sci)
  ti <- i*14
  if(nrow(sci)>0){
    mi <- sum(sci$MINUTES,na.rm=TRUE) ; mi
    fi <- sum(mmi$Best[grep("Fin",mmi$Species)],na.rm=TRUE) ; fi
    hi <- sum(mmi$Best[grep("Hump",mmi$Species)],na.rm=TRUE) ; hi
  }else{
    mi <- 0 ; fi <- hi <- NA
  }
  mri <- data.frame(t=ti,m=mi,fw=fi,hw=hi)  ; mri
  mr <- rbind(mr,mri)
}
mr

# Limit annual range
mr <- mr[mr$t >= 100 & mr$t <= 300,]

# Scale to a rate
mr$fw.r <- mr$fw / mr$m
mr$hw.r <- mr$hw / mr$m

# Scale to move y vals to (0,1)
mr$fw.s <- mr$fw.r / (max(mr$fw.r,na.rm=TRUE)) ; mr$fw.s
mr$hw.s <- mr$hw.r / (max(mr$hw.r,na.rm=TRUE)) ; mr$hw.s

# Add year and platform
mr$year <- 2019
mr$platform <- 'Fin Island'
MR <- rbind(MR, mr)
head(mr)

par(mar=c(4.2,4.2,3,1))
plot(mr$hw.s~mr$t,type="b",ylim=c(0,1),main="Humpback 2019",pch=16,xlab="Day of year",ylab="Encounter rate")
abline(h=1,lty=3)
plot(mr$fw.s~mr$t,type="b",ylim=c(0,1),main="Fin 2019",pch=16,xlab="Day of year",ylab="Encounter rate")
abline(h=1,lty=3)

################################################################################
# 2020
################################################################################

#Bring in data
workdir <- "tests/seasonal/fin/2020/"
lf <- list.files(workdir) ; lf
mm <- read.csv(paste0(workdir,"mm.csv"),stringsAsFactors=FALSE) ; head(mm)
scans <- read.csv(paste0(workdir,"scans.csv"),stringsAsFactors=FALSE) ; head(scans)
seas <- read.csv(paste0(workdir,"sea.csv"),stringsAsFactors=FALSE) ; head(sea)

# Scans - determine good-visibility scans
vis_ok <- c()
i=1
for(i in 1:nrow(scans)){
  (scani <- scans[i,])
  (seasi <- seas %>% dplyr::filter(DATE == scani$DATE, HOUR == scani$HOUR, Vis.km >= 10))
  vis_ok[i] <- ifelse(nrow(seasi)>0, TRUE, FALSE)
}
vis_ok %>% table
scans$vis_ok <- vis_ok

# Marine mammals - determine good-visibility scans
vis_ok <- c()
i=1
for(i in 1:nrow(mm)){
  (mmi <- mm[i,])
  (seasi <- seas %>% dplyr::filter(DATE == mmi$scan.day, HOUR == mmi$scan.hour, Vis.km >= 10))
  vis_ok[i] <- ifelse(nrow(seasi)>0, TRUE, FALSE)
}
vis_ok %>% table
mm$vis_ok <- vis_ok

scans <- scans %>% dplyr::filter(vis_ok == TRUE)
mm <- mm %>% dplyr::filter(vis_ok == TRUE)

# Format other columns
(mm$Best <- as.numeric(as.character(mm$Best)))
(mm$j <- mm$Date %>%
    lubridate::as_date() %>%
    lubridate::yday())
(scans$j <-
    paste0(substr(scans$DATE,1,4),'-',
           substr(scans$DATE,5,6),'-',
           substr(scans$DATE,7,8)) %>%
    lubridate::as_date() %>%
    lubridate::yday())

mm$wk <- floor(mm$j / 14)
scans$wk <- floor(scans$j / 14)

mm$wk %>% range
scans$wk %>% range

head(mm)
head(scans)
unique(mm$Species)

mr <- data.frame()
i=11
for(i in 1:26){
  mmi <- mm[mm$wk==i,] ; head(mmi)
  sci <- scans[scans$wk==i,] ; head(sci)
  ti <- i*14
  if(nrow(sci)>0){
    mi <- sum(sci$MINUTES,na.rm=TRUE) ; mi
    fi <- sum(mmi$Best[grep("Fin",mmi$Species)],na.rm=TRUE) ; fi
    hi <- sum(mmi$Best[grep("Hump",mmi$Species)],na.rm=TRUE) ; hi
  }else{
    mi <- 0 ; fi <- hi <- NA
  }
  mri <- data.frame(t=ti,m=mi,fw=fi,hw=hi)  ; mri
  mr <- rbind(mr,mri)
}
mr

# Limit annual range
mr <- mr[mr$t >= 100 & mr$t <= 300,]

# Scale to a rate
mr$fw.r <- mr$fw / mr$m
mr$hw.r <- mr$hw / mr$m

# Scale to move y vals to (0,1)
mr$fw.s <- mr$fw.r / (max(mr$fw.r,na.rm=TRUE)) ; mr$fw.s
mr$hw.s <- mr$hw.r / (max(mr$hw.r,na.rm=TRUE)) ; mr$hw.s

# Add year and platform
mr$year <- 2020
mr$platform <- 'Fin Island'
MR <- rbind(MR, mr)
head(mr)

par(mar=c(4.2,4.2,3,1))
plot(mr$hw.s~mr$t,type="b",ylim=c(0,1),main="Humpback 2020",pch=16,xlab="Day of year",ylab="Encounter rate")
abline(h=1,lty=3)
plot(mr$fw.s~mr$t,type="b",ylim=c(0,1),main="Fin 2020",pch=16,xlab="Day of year",ylab="Encounter rate")
abline(h=1,lty=3)



################################################################################
# 2021
################################################################################

#Bring in data
workdir <- "tests/seasonal/fin/2021/"
lf <- list.files(workdir) ; lf
mm <- read.csv(paste0(workdir,"mm.csv"),stringsAsFactors=FALSE) ; head(mm)
scans <- read.csv(paste0(workdir,"scans.csv"),stringsAsFactors=FALSE) ; head(scans)
seas <- read.csv(paste0(workdir,"sea.csv"),stringsAsFactors=FALSE) ; head(sea)

# Scans - determine good-visibility scans
vis_ok <- c()
i=1
for(i in 1:nrow(scans)){
  (scani <- scans[i,])
  (seasi <- seas %>% dplyr::filter(DATE == scani$DATE, HOUR == scani$HOUR, Vis.km >= 10))
  vis_ok[i] <- ifelse(nrow(seasi)>0, TRUE, FALSE)
}
vis_ok %>% table
scans$vis_ok <- vis_ok

# Marine mammals - determine good-visibility scans
vis_ok <- c()
i=1
for(i in 1:nrow(mm)){
  (mmi <- mm[i,])
  (seasi <- seas %>% dplyr::filter(DATE == mmi$scan.day, HOUR == mmi$scan.hour, Vis.km >= 10))
  vis_ok[i] <- ifelse(nrow(seasi)>0, TRUE, FALSE)
}
vis_ok %>% table
mm$vis_ok <- vis_ok

scans <- scans %>% dplyr::filter(vis_ok == TRUE)
mm <- mm %>% dplyr::filter(vis_ok == TRUE)

# Format other columns
(mm$Best <- as.numeric(as.character(mm$Best)))
(mm$j <- mm$Date %>%
    lubridate::as_date() %>%
    lubridate::yday())
(scans$j <-
    paste0(substr(scans$DATE,1,4),'-',
           substr(scans$DATE,5,6),'-',
           substr(scans$DATE,7,8)) %>%
    lubridate::as_date() %>%
    lubridate::yday())

mm$wk <- floor(mm$j / 14)
scans$wk <- floor(scans$j / 14)

mm$wk %>% range
scans$wk %>% range

head(mm)
head(scans)
unique(mm$Species)

mr <- data.frame()
i=11
for(i in 1:26){
  mmi <- mm[mm$wk==i,] ; head(mmi)
  sci <- scans[scans$wk==i,] ; head(sci)
  ti <- i*14
  if(nrow(sci)>0){
    mi <- sum(sci$MINUTES,na.rm=TRUE) ; mi
    fi <- sum(mmi$Best[grep("Fin",mmi$Species)],na.rm=TRUE) ; fi
    hi <- sum(mmi$Best[grep("Hump",mmi$Species)],na.rm=TRUE) ; hi
  }else{
    mi <- 0 ; fi <- hi <- NA
  }
  mri <- data.frame(t=ti,m=mi,fw=fi,hw=hi)  ; mri
  mr <- rbind(mr,mri)
}
mr

# Limit annual range
mr <- mr[mr$t >= 100 & mr$t <= 300,]

# Scale to a rate
mr$fw.r <- mr$fw / mr$m
mr$hw.r <- mr$hw / mr$m

# Scale to move y vals to (0,1)
mr$fw.s <- mr$fw.r / (max(mr$fw.r,na.rm=TRUE)) ; mr$fw.s
mr$hw.s <- mr$hw.r / (max(mr$hw.r,na.rm=TRUE)) ; mr$hw.s

# Add year and platform
mr$year <- 2021
mr$platform <- 'Fin Island'
MR <- rbind(MR, mr)
head(mr)

par(mar=c(4.2,4.2,3,1))
plot(mr$hw.s~mr$t,type="b",ylim=c(0,1),main="Humpback 2021",pch=16,xlab="Day of year",ylab="Encounter rate")
abline(h=1,lty=3)
plot(mr$fw.s~mr$t,type="b",ylim=c(0,1),main="Fin 2021",pch=16,xlab="Day of year",ylab="Encounter rate")
abline(h=1,lty=3)

################################################################################
################################################################################
# Master plot

hw <- MR %>%
  dplyr::mutate(species = 'Humpback') %>%
  dplyr::select(platform, year, species, doy = t, minutes = m, count = hw, rate = hw.r, scaled = hw.s)

fw <- MR %>%
  dplyr::mutate(species = 'Fin') %>%
  dplyr::select(platform, year, species, doy = t, minutes = m, count = fw, rate = fw.r, scaled = fw.s)

df <- rbind(hw, fw)


head(df)
ggplot(df, aes(x=doy, y=scaled, color=species)) +
  geom_point(alpha=.8) + geom_line(alpha=.8) +
  scale_color_manual(values = c('cadetblue4', 'darkorange')) +
  xlab('Day of year') + ylab('Encounter rate (scaled)') +
  labs(color='Species') +
  facet_wrap(~year)

write.csv(df, file='tests/seasonal/encounter_rates.csv', row.names=FALSE, quote=FALSE)






