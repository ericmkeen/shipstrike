################################################################################
################################################################################
# Prep seasonal regression
################################################################################
################################################################################

library(dplyr)
library(lubridate)
library(ggplot2)
library(rstanarm)
library(betareg)
library(MASS)

#remotes::install_github("stan-dev/rstanarm")

df <- read.csv('tests/seasonal/encounter_rates.csv',stringsAsFactors=FALSE)
head(df)

df$rate %>% hist

################################################################################
# Prep data

# Filter to species
MR <- df %>% dplyr::filter(species == 'Fin') # also change final line
MR

par(mfrow=c(1,1))
plot(1, xlim=c(0, 365), ylim=c(0,2))
abline(h=1, lty=3)

seasonal_boot <- data.frame()
mri <- MR[MR$minutes > 0,]
mri
i=1
while(i < 5000){
  mrii <- mri[sample(1:nrow(mri),size=nrow(mri),replace=TRUE),]
  #mrii <- mri
  predlm <- mgcv::gam(count ~ s(doy, k=5) + offset(log(minutes)), family='nb',data=mrii)
  #summary(predlm)

  doys <- data.frame(doy=1:359,  minutes=2000)
  doys$month <- ceiling(doys$doy / 30)
  doys
  doys$preds <- predict(predlm, newdata=doys)
  doys$d <- doys$preds / doys$minutes
  doys
  (bang_rate <- doys$d[doys$doy >= 152 & doys$doy <= 244] %>% mean(na.rm=TRUE))
  doys$dev <- doys$d - bang_rate
  doys$d_scaled <- doys$d / bang_rate
  doys
  doys <-
    doys %>%
    group_by(month) %>%
    summarize(doy = round(mean(doy)),
              d = mean(d_scaled),
              mind = min(d_scaled))
  doys
  doys$d[doys$d < 0] <- 0

  if(doys$mind[5] < 1 & doys$mind[11] < 1){

    # Account for unstudied months
    doys$dalt <- doys$d
    doys$dalt[11] <- doys$dalt[10] * .20
    doys$dalt[12] <- doys$dalt[11] * .20
    doys$dalt[1] <- doys$dalt[12] * .20

    doys$dalt[4] <- doys$dalt[5] * .20
    doys$dalt[3] <- doys$dalt[4] * .20
    doys$dalt[2] <- mean(doys$dalt[c(1,3)])
    doys

    i = i + 1
    print(i)
    if(i < 500){
      lines(dalt ~ doy, data=doys, type='l', col=adjustcolor('red',alpha=.1))
    }
    (seasi <- data.frame(iteration=i, month=doys$month, doy = doys$doy, scaled = doys$dalt))
    seasonal_boot <- rbind(seasonal_boot, seasi)
  }
}

nrow(seasonal_boot)
head(seasonal_boot)

save(seasonal_boot,file='tests/fw/seasonal_posterior.RData')

