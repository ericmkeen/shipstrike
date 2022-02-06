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

#remotes::install_github("stan-dev/rstanarm")

df <- read.csv('tests/prep/encounter_rates.csv',stringsAsFactors=FALSE)
head(df)

df$rate %>% hist

################################################################################
# Prep data

# Filter to species
MR <- df %>% dplyr::filter(species == 'Fin') # also change final line
MR

#Create standarized versions as the beta2 precision parameter couldn't cope with raw data
MR$t.s <- with(MR, (doy - mean(doy))/sd(doy))
MR$t.s2 <- MR$t.s^2
MR$scaled <- (MR$scaled / 1.001) + .0001

# Remove NAs
pm <- MR[complete.cases(MR),]
pm <- pm[order(pm$doy),]
nrow(pm)

(mean_log_effort <- pm$minutes %>% log %>% mean) # use this to predict

par(mfrow=c(1,1))
plot(scaled~t.s,data=pm)


################################################################################
# Model

fit1 <- stan_glm.nb(count ~ t.s + t.s2, offset = log(minutes), data = pm, link = 'log',
                    chains = 10, cores = 1, seed = 1, iter = 1000)

#fit1 <- stan_betareg(scaled ~ t.s + t.s2 | t.s, data = pm, link = "logit",link.phi="identity",prior=NULL,
#                     chains = 10, cores = 1, seed = 1, iter = 1000)
fit1

# Posterior ###############
newx <- unique(pm$t.s) %>% sort
newx2 <- newx^2
bp <- posterior_predict(fit1,
                        newdata=data.frame(t.s=newx,t.s2=newx2),
                        offset=rep(mean_log_effort, times=length(newx)))

nrow(bp) ; ncol(bp)
colnames(bp) <- paste0("j",unique(pm$doy)) ; head(bp)

# Scaling Tool for density estimates ################
# Determine the median encounter rate for the typical Bangarang season (June 1- Sept 1) # 152 june 1 - 244 sept 1
mrb <- bp[,3:9]
head(mrb)
medianer <- median(mrb) ; medianer
(bps <- bp / medianer) # Scale biweekly ER by this mean

# Summarize posterior ###############

df <- data.frame(doy=unique(pm$doy) %>% sort)

# Determine confidence intervals
df$mean <- df$median <- df$q2.5 <- df$q5 <- df$q125 <- df$q25 <- df$q75 <- df$q875 <- df$q95 <- df$q975 <- NA
i=1
for(i in 1:ncol(bp)){
  bpi <- bps[,i] ; bpi
  df$mean[i] <- mean(bpi)
  df$median[i] <- median(bpi)
  df$q2.5[i] <- quantile(bpi,.025)
  df$q5[i] <- quantile(bpi,.05)
  df$q125[i] <- quantile(bpi,.125)
  df$q25[i] <- quantile(bpi,.25)
  df$q75[i] <- quantile(bpi,.75)
  df$q875[i] <- quantile(bpi,.875)
  df$q95[i] <- quantile(bpi,.95)
  df$q975[i] <- quantile(bpi,.975)
}
head(df)

# Plot ###############
par(mfrow=c(2,1)) ; par(mar=c(4.4,4.4,2.5,1))
plot(pm$doy,rep(0,times=length(pm$doy)),pch=16,ylim=c(0,20),col="white",cex=.1,
     xlab="Day of year",ylab="Scaled encounter rate",main="Fin whales 2017 - 2021")

px <- c(df$doy,rev(df$doy),df$doy[1]) #; px
py <- c(df$q2.5,rev(df$q975),df$q2.5[1]) #; py
polygon(x=px,y=py,col=adjustcolor("black",alpha.f=.05),border=NA)

px <- c(df$doy,rev(df$doy),df$doy[1]) #; px
py <- c(df$q5,rev(df$q95),df$q5[1]) #; py
polygon(x=px,y=py,col=adjustcolor("black",alpha.f=.05),border=NA)

px <- c(df$doy,rev(df$doy),pm$doy[1]) #; px
py <- c(df$q125,rev(df$q875),df$q125[1]) #; py
polygon(x=px,y=py,col=adjustcolor("black",alpha.f=.05),border=NA)

px <- c(df$doy,rev(df$doy),df$doy[1]) #; px
py <- c(df$q25,rev(df$q75),df$q25[1]) #; py
polygon(x=px,y=py,col=adjustcolor("black",alpha.f=.05),border=NA)

lines(df$median~df$doy,lwd=3,type="l",col="grey50")
abline(h=1,lty=3)


# Results object ###############

pm # data used
bp # raw posterior
bps # scaled posterior (used to scale Bangarang density estimates)
df # summary of posterior

#HW <- list(data=pm,model=fit1,posterior.raw=bp,posterior=bps,summary=df)
#HW
#save(HW,file='tests/prep/posterior-hw.RData')

MR <- list(data=pm,model=fit1,posterior.raw=bp,posterior=bps,summary=df)
MR
save(MR,file='tests/fw/seasonal_posterior.RData')








