################################################################################
# p(whale)
################################################################################

library(dplyr)
library(Distance)
library(dsm)
library(ggplot2)
library(raster)
library(spatstat.geom)
library(data.table)
library(bangarang)
library(devtools)
#install_github('r-spatial/gstat')
#library(gstat)

################################################################################
################################################################################
# Prep data

data(segments_5km)
segments <- segments_5km
head(segments)

data("whale_sightings")
sightings <- whale_sightings
head(sightings)

# Sanity check
segs <- sightings$seg_id[sightings$spp %in% c('HW','BW')]
segs <- segments %>% dplyr::filter(Sample.Label %in% segs)
par(mfrow=c(1,1)) ; plotKFS()
points(x=segs$x, y=segs$y)

# Format region labels
segments <- segments %>% dplyr::filter(block != '')
segments$Region.Label <- segments$block
segments$Region.Label[segments$block %in% c('CAA','CAM')] <- 'Caamano'
segments$Region.Label[segments$block %in% c('CMP')] <- 'Campania'
segments$Region.Label[segments$block %in% c('SQU')] <- 'Squally'
segments$Region.Label[segments$block %in% c('EST')] <- 'Estevan'
#segments$Region.Label[segments$block %in% c('VER','WHA','WRI','MCK')] <- 'Other'
segments$Region.Label %>% table

sightings <- sightings %>% dplyr::filter(block != '')
sightings$Region.Label <- sightings$block
sightings$Region.Label[sightings$block %in% c('CAA','CAM')] <- 'Caamano'
sightings$Region.Label[sightings$block %in% c('CMP')] <- 'Campania'
sightings$Region.Label[sightings$block %in% c('SQU')] <- 'Squally'
sightings$Region.Label[sightings$block %in% c('EST')] <- 'Estevan'
#sightings$Region.Label[sightings$block %in% c('VER','WHA','WRI','MCK')] <- 'Other'
sightings$Region.Label %>% table

# Format day of year
segments$month <- lubridate::month(segments$datetime)
segments$doy <- lubridate::yday(segments$datetime)
sightings$doy <- lubridate::yday(sightings$datetime)

# Format table with sightings
sightings %>% head
(data_table <-
    sightings %>%
    dplyr::filter(spp %in% c('HW')) %>%
    dplyr::transmute(distance = distance,
                     size = size,
                     Sample.Label = seg_id,
                     Region.Label = Region.Label,
                     year = substr(day,1,4),
                     doy = doy,
                     block = block,
                     bft = bft,
                     z = z,
                     zrange = zrange,
                     zsd = zsd) %>%
    dplyr::mutate(object = 1:dplyr::n()))

# Format table linking sightings to segments
(obs_table <- data_table %>% dplyr::select(object, Region.Label, Sample.Label))

# Format sample (segments) table
segments %>% head
sample_table <- segments


################################################################################
################################################################################
# Fit detection functions

data_table %>% head
obs_table %>% head
sample_table %>% head

# 10%  truncation distance is ~2.7km
data_table$distance
quantile(data_table$distance, 0.905, na.rm=TRUE)

dso1 <- Distance::ds(data = data_table, truncation = 2.7,
                     formula= ~1,
                     key = 'hn', quiet=FALSE)

dso2 <- Distance::ds(data = data_table, truncation = 2.7,
                     formula= ~1 + bft,
                     adjustment = NULL, key = 'hn', quiet=FALSE)

dso3 <- Distance::ds(data = data_table, truncation = 2.7,
                     formula= ~1 + factor(year),
                     adjustment = NULL, key = 'hn', quiet=FALSE)

Distance::summarize_ds_models(dso1, dso2, dso3)

save(dso1, dso2, dso3, file='tests/hw/ds-models.RData')

plot(dso3)

################################################################################
################################################################################
# Then fit DSM

dso_keep <- dso3

# Simply xy - select family

# quasi-poisson
dsm.qp.xy <- dsm(density.est~s(x,y, k=12), dso_keep, sample_table, data_table, method="REML")
summary(dsm.qp.xy)
vis.gam(dsm.qp.xy, plot.type="contour", view=c("x","y"), asp=1, type="response", contour.col="black", n.grid=100)
par(mfrow=c(2,2), mar=c(4.2,4.2,3,.5)) ; gam.check(dsm.qp.xy) ; par(mfrow=c(1,1))

# negative binomial
dsm.nb.xy <- dsm(density.est~s(x,y, k=12), dso_keep, sample_table, data_table, family=nb(), method="REML")
summary(dsm.nb.xy)
vis.gam(dsm.nb.xy, plot.type="contour", view=c("x","y"), asp=1, type="response", contour.col="black", n.grid=100)
par(mfrow=c(2,2), mar=c(4.2,4.2,3,.5)) ; gam.check(dsm.nb.xy) ; par(mfrow=c(1,1))

# tweedie
dsm.tw.xy <- dsm(density.est~s(x,y, k=12), dso_keep, sample_table, data_table, family=tw(), method="REML")
summary(dsm.tw.xy)
vis.gam(dsm.tw.xy, plot.type="contour", view=c("x","y"), asp=1, type="response", contour.col="black", n.grid=100)
par(mfrow=c(2,2), mar=c(4.2,4.2,3,.5)) ; gam.check(dsm.tw.xy) ; par(mfrow=c(1,1))

# tweedie gives best gg fit.
# let k be automatically selected

# removing zsd, since it is closely correlated with zrange
plot(zsd ~ zrange, data=sample_table)
plot(z ~ zrange, data=sample_table) # not correlated

# Round 1 model fitting
dsm1.xyd_interaction <- dsm(density.est~s(x,y,doy), dso_keep, sample_table, data_table, family=tw(), method="REML")
AIC(dsm1.xyd_interaction)

dsm1.xy <- dsm(density.est~s(x,y), dso_keep, sample_table, data_table, family=tw(), method="REML")
summary(dsm1.xy)
vis.gam(dsm1.xy, plot.type="contour", view=c("x","y"), asp=1, type="response", contour.col="black", n.grid=100, zlim=c(0,.2))
par(mfrow=c(2,2), mar=c(4.2,4.2,3,.5)) ; gam.check(dsm1.xy) ; par(mfrow=c(1,1))
AIC(dsm1.xy)

dsm1.xyz <- dsm(density.est~s(x,y) + s(z), dso_keep, sample_table, data_table, family=tw(), method="REML")
summary(dsm1.xyz)
vis.gam(dsm1.xyz, plot.type="contour", view=c("x","y"), asp=1, type="response", contour.col="black", n.grid=100, zlim=c(0,.2))
par(mfrow=c(2,2), mar=c(4.2,4.2,3,.5)) ; gam.check(dsm1.xyz) ; par(mfrow=c(1,1))
AIC(dsm1.xyz)

dsm1.xyzrange <- dsm(density.est~s(x,y) + s(zrange), dso_keep, sample_table, data_table, family=tw(), method="REML")
summary(dsm1.xyzrange)
vis.gam(dsm1.xyzrange, plot.type="contour", view=c("x","y"), asp=1, type="response", contour.col="black", n.grid=100, zlim=c(0,.2))
par(mfrow=c(2,2), mar=c(4.2,4.2,3,.5)) ; gam.check(dsm1.xyzrange) ; par(mfrow=c(1,1))
AIC(dsm1.xyzrange)

dsm1.xyy <- dsm(density.est~s(x,y) + year, dso_keep, sample_table, data_table, family=tw(), method="REML")
summary(dsm1.xyy)
vis.gam(dsm1.xyy, plot.type="contour", view=c("x","y"), asp=1, type="response", contour.col="black", n.grid=100, zlim=c(0,.2))
par(mfrow=c(2,2), mar=c(4.2,4.2,3,.5)) ; gam.check(dsm1.xyy) ; par(mfrow=c(1,1))
AIC(dsm1.xyy)

dsm1.xyd <- dsm(density.est~s(x,y) + s(doy), dso_keep, sample_table, data_table, family=tw(), method="REML")
summary(dsm1.xyd)
vis.gam(dsm1.xyd, plot.type="contour", view=c("x","y"), asp=1, type="response", contour.col="black", n.grid=100, zlim=c(0,.2))
par(mfrow=c(2,2), mar=c(4.2,4.2,3,.5)) ; gam.check(dsm1.xyd) ; par(mfrow=c(1,1))
AIC(dsm1.xyd)

dsm1.xyb <- dsm(density.est~s(x,y) + block, dso_keep, sample_table, data_table, family=tw(), method="REML")
summary(dsm1.xyb)
vis.gam(dsm1.xyb, plot.type="contour", view=c("x","y"), asp=1, type="response", contour.col="black", n.grid=100, zlim=c(0,.2))
par(mfrow=c(2,2), mar=c(4.2,4.2,3,.5)) ; gam.check(dsm1.xyb) ; par(mfrow=c(1,1))
AIC(dsm1.xyb)

dsm1.xyd_interaction %>% AIC
dsm1.xyd %>% AIC # DO use day of year
dsm1.xyzrange %>% AIC
dsm1.xyb %>% AIC
dsm1.xyz %>% AIC
dsm1.xyy %>% AIC
dsm1.xy %>% AIC

# Round 2
dsm2.z <- dsm(density.est~s(x,y,doy) + s(z), dso_keep, sample_table, data_table, family=tw(), method="REML")
dsm2.zrange <- dsm(density.est~s(x,y,doy) + s(zrange), dso_keep, sample_table, data_table, family=tw(), method="REML")
dsm2.y <- dsm(density.est~s(x,y,doy) + year, dso_keep, sample_table, data_table, family=tw(), method="REML")

dsm2.z %>% AIC
dsm1.xyd_interaction %>% AIC
dsm2.y %>% AIC
dsm2.zrange %>% AIC

# Round 3
dsm3.zrange <- dsm(density.est~s(x,y,doy) + s(z) + s(zrange), dso_keep, sample_table, data_table, family=tw(), method="REML")
dsm3.y <- dsm(density.est~s(x,y,doy) + s(z) + year, dso_keep, sample_table, data_table, family=tw(), method="REML")

dsm3.zrange %>% AIC
dsm3.y %>% AIC
dsm2.z %>% AIC

# Round 4
dsm4.y <- dsm(density.est~te(x,y,doy) + s(z) + s(zrange) + year, dso_keep, sample_table, data_table, family=tw(), method="REML")
plot(dsm4.y)

dsm4.y %>% AIC
dsm3.zrange %>% AIC # Delta AIC = 14

# Winner:
dsm4.y
dsm4.y %>% summary

vis.gam(dsm4.y, plot.type="contour", view=c("x","y"), asp=1, type="response", contour.col="black", n.grid=100, zlim=c(0,.5))


################################################################################
# Create density surface

#dsm_keep <- dsm.tw.xy
dsm_keep <- dsm4.y

# Load prediction grid
grids <- read.csv('tests/grid-kfs-1km.csv', stringsAsFactors=FALSE)

predict_surface <- function(dsm_keep, sample_table, grids, cex_scale = 3, toplot=TRUE){

  #dsm_keepi <- dsm_keep
  #dsm_keepi <- dsm_bs

  # Predict on samples
  d_mean <- predict(dsm_keep, sample_table, off.set=(sample_table$Effort))
  #hist(d_mean)
  #plotKFS()
  #points(x=sample_table$x, y=sample_table$y, cex=d_mean*10)

  # Prepare raster
  df <- data.frame(x=sample_table$x, y=sample_table$y, z= d_mean)
  df <- df[complete.cases(df),] ; nrow(df)
  e <- extent(df[,1:2])
  r <- raster(e,ncol=150, nrow=150)
  x <- rasterize(df[, 1:2], r, df[,3], fun=mean)
  #plot(x)

  # Interpolate with inverse distance weighting
  library(spatstat.core)
  library(spatstat.geom)
  dfwin <- owin(xrange = range(grid_kfs$x), yrange = range(grid_kfs$y))
  dfppp <- ppp(x = df$x, y=df$y, window = dfwin, marks = df$z)
  xi <- idw(dfppp, at='pixels')
  class(xi)
  #plot(xi)

    #mg <- gstat(formula = z~1, locations = ~x+y, data=df,
  #            nmax=7, nmin=3,set=list(idp = 0))
  #suppressWarnings({ suppressMessages({
  #  xi <- interpolate(x, mg, debug.level=0)
  #}) })
  #class(xi)
  #plot(xi)

  #  Prepare fine-scale grid of densities
  xdf <- as.data.frame(xi) ; head(xdf)
  z <- as.data.frame(xi) ; z %>% head
  names(z)[3] <- 'z'
  z %>% head
  nrow(z)
  nrow(xdf)
  xdf$value <- z$z
  xdf %>% head
  mean(xdf$value)

  # Now use fine-scale raster to find predicted density at each grids location
  dist <- function(a, b){
    dt <- data.table((df2$x-a)^2+(df2$y-b)^2)
    return(which.min(dt$V1))}
  df1 <- data.table(x=grids$x, y=grids$y) ; head(df1)
  df2 <- data.table(x=xdf$x, y=xdf$y) ; head(df2)
  results <- df1[, j = list(Closest =  dist(x, y)), by = 1:nrow(df1)]
  grids$d_mean <- xdf$value[results$Closest]

  # Test plot
  #hist(grids$d_mean)
  #mean(grids$d_mean, na.rm=TRUE)
  if(toplot){
    cexmax <- max(grids$d_mean, na.rm=TRUE)
    (cexes <- (grids$d_mean / cexmax) * cex_scale)
    plotKFS()
    points(x=grids$x, y=grids$y, cex=cexes) #sqrt(grids$d_mean)*cex_scale)
  }

  return(grids$d_mean)
}

grids$month <- 0
d_mean <- predict_surface(dsm_keep, sample_table, grids)
grids$d_mean <- d_mean
grids$d_meanr <- d_mean / sum(d_mean)
head(grids)

# Explore seasonal effect
grid_template <- grids
grid_mr <- grids

par(mfrow=c(2,2))

# June
head(sample_table)
d_mean <- predict_surface(dsm_keep, sample_table %>% dplyr::filter(month == 6), grids) # june 15
gridi <- grid_template
gridi$month <- 6
gridi$d_mean <- d_mean
gridi$d_meanr <- d_mean / sum(d_mean)
grid_mr <- rbind(grid_mr, gridi)

# July
d_mean <- predict_surface(dsm_keep, sample_table %>% dplyr::filter(month == 7), grids) # july 15
gridi <- grid_template
gridi$month <- 7
gridi$d_mean <- d_mean
gridi$d_meanr <- d_mean / sum(d_mean)
grid_mr <- rbind(grid_mr, gridi)

# August 15
d_mean <- predict_surface(dsm_keep, sample_table %>% dplyr::filter(month == 8), grids) # aug 15
gridi <- grid_template
gridi$month <- 8
gridi$d_mean <- d_mean
gridi$d_meanr <- d_mean / sum(d_mean)
grid_mr <- rbind(grid_mr, gridi)

# September 15
d_mean <- predict_surface(dsm_keep, sample_table %>% dplyr::filter(month == 9), grids) # sep 15
gridi <- grid_template
gridi$month <- 9
gridi$d_mean <- d_mean
gridi$d_meanr <- d_mean / sum(d_mean)
grid_mr <- rbind(grid_mr, gridi)

head(grid_mr)

# Save results
grids <- grid_mr
save(grids, file='tests/hw/dsm-estimate.RData')
save(dsm_keep, file='tests/hw/dsm-model.RData')

hist(grids$d_mean)

head(grids)
grids %>%
  group_by(month, block) %>%
  summarize(D = round(mean(d_mean),3), Dr = round(mean(d_meanr),4)) %>%
  as.data.frame()

################################################################################
################################################################################
################################################################################
################################################################################
# Attempt CV bootstrapping

B <- 1000

data_table %>% head
sample_table %>% head

data_bs <- data_table[complete.cases(data_table),] ; data_bs %>% nrow
data_bs$month <- lubridate::month(strptime(data_bs$doy,format='%j'))
data_bs$month[data_bs$month == 5] <- 6
table(data_bs$month)
head(data_bs)

sample_bs <- sample_table[complete.cases(sample_table),] ; sample_bs %>% nrow
sample_bs$month <- lubridate::month(sample_bs$datetime)
sample_bs$month[sample_bs$month == 5] <- 6

bootstraps <- data.frame()
i=1
for(i in 1:B){
  message('=========================================\n BOOSTRAP ',i,'\n=========================================')

  #=============================================================================
  message('--- Preparing bootstrap dataset ...')
  sample_bs %>% head
  table(sample_bs$month)
  #(seg_bs <- sample(sample_bs$Sample.Label, size=nrow(sample_bs),replace=TRUE))

  # Bootstrap resample, maintaining monthly proportion of effort
  seg_bs <- c()
  monthi <- 6
  for(monthi in unique(sample_bs$month)){
    (seg_bs_monthi <- sample_bs %>% dplyr::filter(month == monthi))
    (seg_bs_monthii <- sample(seg_bs_monthi$Sample.Label, size=nrow(seg_bs_monthi),replace=TRUE))
    seg_bs <- c(seg_bs, seg_bs_monthii)
  }
  seg_bs

  data_bsi <- data.frame()
  sample_bsi <- data.frame()
  i=1
  for(si in 1:length(seg_bs)){
    (segi <- seg_bs[si])
    (sampli <- sample_bs %>% dplyr::filter(Sample.Label == segi))
    sampli$Sample.Label <- si
    sample_bsi <- rbind(sample_bsi, sampli)
    (siti <- data_bs %>% dplyr::filter(Sample.Label == segi))
    if(nrow(siti)>0){
      siti$Sample.Label <- si
      data_bsi <- rbind(data_bsi, siti)
    }
  }
  table(sample_bsi$month)
  table(data_bsi$month)

  nrow(data_bsi)
  if(nrow(data_bsi)<=0){
    data_bsi <- data.frame(Sample.Label = NA, Effort = NA, x = NA, y = NA, datetime = NA,
                           year = NA, day = NA, block = NA, z = NA, zmin = NA, zmax = NA, zsd = NA, zrange = NA, Region.Label = NA, doy = NA)
    (data_bsi <- data_bsi[0,])
  }
  data_bsi

  #=============================================================================
  message('--- Fitting detection function ...')
  suppressWarnings({suppressMessages({
    dso_bs <- Distance::ds(data = data_bs, truncation = 2.0, formula= ~1 + factor(year), key = 'hn', quiet=FALSE)
    #plot(dso_bs)
  }) })

  #=============================================================================
  message('--- Fitting spatial density model ...')
  suppressWarnings({
    dsm_bs <- dsm(dsm_keep$formula,
                  dso_bs, sample_bsi, data_bsi, family=tw(), method="REML")
  })

  #=============================================================================
  # Predict on samples

  message('--- Predicting density surface ...')
  message('--- --- entire season (averaged) ...')
  par(mfrow=c(2,3))
  d_mean <- predict_surface(dsm_bs, sample_bsi, grids, toplot=TRUE)
  text(x=-129.62, y=53.48, labels='Overall', col='red', cex=1.5, pos=4)

  d_mean %>% length
  nrow(grids)
  bs_resulti <- data.frame(grid_id = grids$id, iteration = i, month = 0, D = d_mean, Dr = d_mean/sum(d_mean))
  head(bs_resulti)

  #=============================================================================
  # Predict on samples -- for each month

  #par(mfrow=c(2,2))
  months <- 6:9
  #         6/15  7/15  8/15  9/15
  #doys <- c(166,  196,  227,  258)
  mi=3
  for(mi in 1:length(months)){
    (monthi <- months[mi])
    #(doyi <- doys[di])
    #(monthi <- di + 5) #(1 + floor(doyi/30)))
    message('--- --- month ',monthi,' ...')
    d_mean <- NA
    (sampli <- sample_bsi %>% dplyr::filter(month == monthi)) %>% nrow
    sampli$month %>% table
    try({
      d_mean <- predict_surface(dsm_bs, sampli, cex_scale = 1, grids, toplot=TRUE)
      text(x=-129.62, y=53.48, labels=paste0('Month ',monthi), col='red', cex=1.5, pos=4)
    }, silent=TRUE)
    if(is.na(d_mean[1])){message('--- --- --- DSM prediction failed! Returning NA for this month ...')}
    #hist(d_mean, main=monthi)
    bs_resultii <- data.frame(grid_id = grids$id, iteration = i, month = monthi, D = d_mean, Dr = d_mean/sum(d_mean))
    head(bs_resultii)
    bs_resulti <- rbind(bs_resulti, bs_resultii)
  }

  par(mfrow=c(1,1))

  #=============================================================================
  # Saving result

  bootstraps <- rbind(bootstraps, bs_resulti)
  message('')
  save(bootstraps, file='tests/hw/dsm-bootstraps.RData')
}


################################################################################

nrow(bootstraps)

#bootstraps$D %>% hist(breaks=20)


d_grid <-
  bootstraps %>%
  dplyr::group_by(month, grid_id) %>%
  dplyr::summarize(d_mean = mean(D, na.rm=TRUE),
                   d_sd = sd(D, na.rm=TRUE)) %>%
  dplyr::mutate(d_cv = d_sd / d_mean)

d_grid %>% head
d_grid$d_mean %>% hist


plotKFS()
points(x=grids$x, y=grids$y, cex=d_grid$d_mean*5)

d_grid %>%
  dplyr::group_by(month) %>% summarize(d_mean = mean(d_mean, na.rm=TRUE),
                                       d_cv = mean(d_cv, na.rm=TRUE))

# define fin island viewshed
filter2fin <- function(grids, return_id=TRUE){
  #plotKFS()
  #points(x=grids$x, y=grids$y, pch=16, cex=.2)
  gridfin <- grids %>% dplyr::filter(y >= 53.16,
                                     y <= 53.32,
                                     x <= -129.34,
                                     x >= -129.53)
  #points(x=gridfin$x, y=gridfin$y, pch=16, cex=.4, col='red')
  if(return_id){
    return(gridfin$id)
  }else{
    return(gridfin)
  }
}

(fin_ids <- filter2fin(grids))

d_grid %>%
  dplyr::mutate(fin = ifelse(grid_id %in% fin_ids,TRUE,FALSE)) %>%
  dplyr::group_by(month, fin) %>%
  dplyr::summarize(D = mean(d_mean)) %>%
  dplyr::group_by(month) %>%
  dplyr::summarize(D_ratio = D[fin==TRUE] / D[fin==FALSE])

d_gridi <- d_grid %>% dplyr::filter(month == 0)
d_gridi %>% nrow
(d_tot <- sum(d_gridi$d_mean))
d_gridi$d_prop <- d_gridi$d_mean / d_tot
d_gridi$d_prop %>% sum


plotKFS()
points(x=grids$x, y=grids$y, pch=16, cex=d_gridi$d_prop*300)



################################################################################

# (1) Use monthly Bangarang surfaces to estimate relative distribution (sum to 1) (CV for each cell)
# (2) Pool all months to get an AVERAGE (CV) DENSITY across the Bangarang surface for June - September
# (3) Scale that density by the seasonal curve to get a density in each month

# How to handle Squally v elsewhere differences in the monthly trend?
# Decision: just mention, refer to exercise in which we compared north squally density to average
# density elsewhere and the ratio ranged only 5% (1.21:1 to 1.26:1, N Squally higher) for months 6 - 9,
# so we are not going to address it here.

# Practically speaking:
# Bootstrap 1000x (1000 estimates of season average, 1000 of each month's density distribution).
# save as RData

# Separate script:
# Develop distributions for each grid cell (1000 draws for each grid cell -->  1409 x 1000)

#



