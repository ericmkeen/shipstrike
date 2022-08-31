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

################################################################################
################################################################################
# Prep data

data(segments_5km)
segments <- segments_5km
head(segments)
segments$Effort %>% mean
segments$Effort %>% sum
segments$Effort[segments$Effort < 10] %>% sum
segments %>% dplyr::group_by(year) %>% dplyr::summarize(km = sum(Effort[Effort < 10]))
nrow(segments)

# Summarize effort
segments %>%
  dplyr::group_by(year) %>%
  dplyr::summarize(n = n(),
                   km = sum(Effort),
                   km10 = sum(Effort[Effort < 10]))

# Sightings
data("whale_sightings")
sightings <- whale_sightings

head(sightings)
nrow(segments)

# Summarize sightings
sightings %>%
  dplyr::mutate(year = lubridate::year(datetime)) %>%
  dplyr::group_by(year, spp) %>%
  dplyr::summarize(n_tot = n(),
                   n_valid = length(which(!is.na(distance)))) %>%
  group_by(spp) #%>%
  #summarize(n_tot = sum(n_tot),
  #          n_valid = sum(n_valid))

# Sanity check
segs <- sightings$seg_id[sightings$spp %in% c('FW','BW')]
segs <- segments %>% dplyr::filter(Sample.Label %in% segs)
plotKFS()
points(x=segs$x, y=segs$y)

# Nice plot
pdf('tests/figs/lta/effort.pdf', width=6, height=9)
plotKFS()
points(x=segments$x, y=segments$y, cex=.7, pch=16, col=adjustcolor('black', alpha.f = .4))
dev.off()

pdf('tests/figs/lta/fw.pdf', width=6, height=9)
plotKFS()
fw <- sightings %>% dplyr::filter(spp=='FW')
points(x=fw$x, y=fw$y, cex=sqrt(fw$size), pch=16, col=adjustcolor('darkblue',alpha.f=.6))
dev.off()

pdf('tests/figs/lta/hw.pdf', width=6, height=9)
plotKFS()
hw <- sightings %>% dplyr::filter(spp=='HW')
points(x=hw$x, y=hw$y, cex=sqrt(hw$size), pch=16, col=adjustcolor('darkblue',alpha.f=.6))
dev.off()

pdf('tests/figs/lta/key.pdf', width=6, height=9)
plotKFS()
points(x=rep(-129.6, times=4),
       y=seq(52.82, 52.92, length=4),
       cex = sqrt(c(1,2,4,10)),
       pch=16, col=adjustcolor('darkblue',alpha.f=.6))
dev.off()

################################################################################

# Format region labels
segments <- segments %>% dplyr::filter(block != '')
segments$Region.Label <- segments$block
segments$Region.Label[segments$block %in% c('CAA','CAM')] <- 'Caamano'
segments$Region.Label[segments$block %in% c('CMP')] <- 'Campania'
segments$Region.Label[segments$block %in% c('SQU')] <- 'Squally'
segments$Region.Label[segments$block %in% c('EST')] <- 'Estevan'
segments$Region.Label[segments$block %in% c('VER','WHA','WRI','MCK')] <- 'Other'
segments$Region.Label %>% table

sightings <- sightings %>% dplyr::filter(block != '')
sightings$Region.Label <- sightings$block
sightings$Region.Label[sightings$block %in% c('CAA','CAM')] <- 'Caamano'
sightings$Region.Label[sightings$block %in% c('CMP')] <- 'Campania'
sightings$Region.Label[sightings$block %in% c('SQU')] <- 'Squally'
sightings$Region.Label[sightings$block %in% c('EST')] <- 'Estevan'
sightings$Region.Label[sightings$block %in% c('VER','WHA','WRI','MCK')] <- 'Other'
sightings$Region.Label %>% table

# Format day of year
segments$doy <- lubridate::yday(segments$datetime)
sightings$doy <- lubridate::yday(sightings$datetime)

# Format table with sightings
sightings %>% head
(data_table <-
    sightings %>%
    dplyr::filter(spp == 'FW') %>%
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

nrow(data_table)
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

save(dso1, dso2, dso3, file='tests/fw/ds-models.RData')

par(mfrow=c(1,1), mar=c(4.3,4.3,.5,.5))
plot(dso1)


################################################################################
################################################################################
# Then fit DSM

dso_keep <- dso1

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

# Let's avoid day of year & year

# Round 1 model fitting

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

#dsm1.xyy <- dsm(density.est~s(x,y) + year, dso_keep, sample_table, data_table, family=tw(), method="REML")
#summary(dsm1.xyy)
#vis.gam(dsm1.xyy, plot.type="contour", view=c("x","y"), asp=1, type="response", contour.col="black", n.grid=100, zlim=c(0,.2))
#par(mfrow=c(2,2), mar=c(4.2,4.2,3,.5)) ; gam.check(dsm1.xyy) ; par(mfrow=c(1,1))
#AIC(dsm1.xyy)

#dsm1.xyd <- dsm(density.est~s(x,y) + s(doy), dso_keep, sample_table, data_table, family=tw(), method="REML")
#summary(dsm1.xyd)
#vis.gam(dsm1.xyd, plot.type="contour", view=c("x","y"), asp=1, type="response", contour.col="black", n.grid=100, zlim=c(0,.2))
#par(mfrow=c(2,2), mar=c(4.2,4.2,3,.5)) ; gam.check(dsm1.xyd) ; par(mfrow=c(1,1))
#AIC(dsm1.xyd)

dsm1.xyb <- dsm(density.est~s(x,y) + block, dso_keep, sample_table, data_table, family=tw(), method="REML")
summary(dsm1.xyb)
vis.gam(dsm1.xyb, plot.type="contour", view=c("x","y"), asp=1, type="response", contour.col="black", n.grid=100, zlim=c(0,.2))
par(mfrow=c(2,2), mar=c(4.2,4.2,3,.5)) ; gam.check(dsm1.xyb) ; par(mfrow=c(1,1))
AIC(dsm1.xyb)

#dsm1.xyd %>% AIC # let's avoid day of year for now
dsm1.xyzrange %>% AIC
dsm1.xyz %>% AIC
dsm1.xyb %>% AIC
#dsm1.xyy %>% AIC
dsm1.xy %>% AIC

# Round 2
dsm2.z <- dsm(density.est~s(x,y) + s(zrange) + s(z), dso_keep, sample_table, data_table, family=tw(), method="REML")
summary(dsm2.z)
vis.gam(dsm2.z, plot.type="contour", view=c("x","y"), asp=1, type="response", contour.col="black", n.grid=100, zlim=c(0,.5))
par(mfrow=c(2,2), mar=c(4.2,4.2,3,.5)) ; gam.check(dsm2.z) ; par(mfrow=c(1,1))
AIC(dsm2.z)

#dsm2.zrange <- dsm(density.est~s(x,y) + s(doy) + s(zrange), dso_keep, sample_table, data_table, family=tw(), method="REML")
#summary(dsm2.zrange)
#vis.gam(dsm2.zrange, plot.type="contour", view=c("x","y"), asp=1, type="response", contour.col="black", n.grid=100, zlim=c(0,.5))
#par(mfrow=c(2,2), mar=c(4.2,4.2,3,.5)) ; gam.check(dsm2.zrange) ; par(mfrow=c(1,1))
#AIC(dsm2.zrange)

dsm2.b <- dsm(density.est~s(x,y) + s(zrange) + block, dso_keep, sample_table, data_table, family=tw(), method="REML")
summary(dsm2.b)
vis.gam(dsm2.b, plot.type="contour", view=c("x","y"), asp=1, type="response", contour.col="black", n.grid=100, zlim=c(0,.2))
par(mfrow=c(2,2), mar=c(4.2,4.2,3,.5)) ; gam.check(dsm2.b) ; par(mfrow=c(1,1))
AIC(dsm2.b)

#dsm2.y <- dsm(density.est~s(x,y) + s(zrange) + year, dso_keep, sample_table, data_table, family=tw(), method="REML")
#summary(dsm2.y)
#vis.gam(dsm2.y, plot.type="contour", view=c("x","y"), asp=1, type="response", contour.col="black", n.grid=100, zlim=c(0,.2))
#par(mfrow=c(2,2), mar=c(4.2,4.2,3,.5)) ; gam.check(dsm2.y) ; par(mfrow=c(1,1))
#AIC(dsm2.y)

dsm2.z %>% AIC
dsm2.b %>% AIC
#dsm2.y %>% AIC
dsm1.xyzrange %>% AIC

# Round 3
dsm3.b <- dsm(density.est~s(x,y) + s(zrange) + s(z) + factor(block), dso_keep, sample_table, data_table, family=tw(), method="REML")
summary(dsm3.b)
vis.gam(dsm3.b, plot.type="contour", view=c("x","y"), asp=1, type="response", contour.col="black", n.grid=100, zlim=c(0,.5))
par(mfrow=c(2,2), mar=c(4.2,4.2,3,.5)) ; gam.check(dsm3.b) ; par(mfrow=c(1,1))
AIC(dsm3.b)

#dsm3.y <- dsm(density.est~s(x,y) + s(zrange) + s(doy) + year, dso_keep, sample_table, data_table, family=tw(), method="REML")
#summary(dsm3.y)
#vis.gam(dsm3.y, plot.type="contour", view=c("x","y"), asp=1, type="response", contour.col="black", n.grid=100, zlim=c(0,.5))
#par(mfrow=c(2,2), mar=c(4.2,4.2,3,.5)) ; gam.check(dsm3.y) ; par(mfrow=c(1,1))
#AIC(dsm3.y)

dsm2.z %>% AIC
dsm3.b %>% AIC
#dsm3.y %>% AIC

# Round 4
#dsm4.y <- dsm(density.est~s(x,y) + s(zrange) + s(doy) + s(z) + year, dso_keep, sample_table, data_table, family=tw(), method="REML")
#summary(dsm4.y)
#vis.gam(dsm4.y, plot.type="contour", view=c("x","y"), asp=1, type="response", contour.col="black", n.grid=100, zlim=c(0,.5))
#par(mfrow=c(2,2), mar=c(4.2,4.2,3,.5)) ; gam.check(dsm4.y) ; par(mfrow=c(1,1))
#AIC(dsm4.y)

#dsm4.b <- dsm(density.est~s(x,y) + s(zrange) + s(doy) + block + s(z), dso_keep, sample_table, data_table, family=tw(), method="REML")
#summary(dsm4.b)
#vis.gam(dsm4.b, plot.type="contour", view=c("x","y"), asp=1, type="response", contour.col="black", n.grid=100, zlim=c(0,.5))
#par(mfrow=c(2,2), mar=c(4.2,4.2,3,.5)) ; gam.check(dsm4.b) ; par(mfrow=c(1,1))
#AIC(dsm4.b)

#dsm3.z %>% AIC
#dsm4.b %>% AIC
#dsm4.y %>% AIC

# Round 5
#dsm5.y <- dsm(density.est~s(x,y) + s(zrange) + s(doy) + block + s(z) + year, dso_keep, sample_table, data_table, family=tw(), method="REML")
#summary(dsm5.y)
#vis.gam(dsm5.y, plot.type="contour", view=c("x","y"), asp=1, type="response", contour.col="black", n.grid=100, zlim=c(0,.5))
#par(mfrow=c(2,2), mar=c(4.2,4.2,3,.5)) ; gam.check(dsm5.y) ; par(mfrow=c(1,1))
#AIC(dsm5.y)

#dsm4.z %>% AIC
#dsm5.y %>% AIC

# Winners:
#dsm4.z %>% summary
#dsm5.y %>% summary
# carrying dsm4.z forward, since it has same AIC but fewer parameters
#dsm3.z %>% AIC
#dsm3.z %>% summary
dsm2.z %>% AIC
dsm2.z %>% summary

# Base model
vis.gam(dsm.tw.xy, plot.type="contour", view=c("x","y"), asp=1, type="response", contour.col="black", n.grid=100)

# Best model
vis.gam(dsm2.z, plot.type="contour", view=c("x","y"), asp=1, type="response", contour.col="black", n.grid=100, zlim=c(0,.5))


################################################################################
# Create density surface

#dsm_keep <- dsm.tw.xy
#dsm_keep <- dsm4.z
dsm_keep <- dsm2.z

# Load prediction grid
grids <- read.csv('tests/grid-kfs-1km.csv', stringsAsFactors=FALSE)

predict_surface <- function(dsm_keep, sample_table, grids, cex_scale = 2, toplot=TRUE){
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
  mg <- gstat(formula = z~1, locations = ~x+y, data=df,
              nmax=7, nmin=3,set=list(idp = 0))
  suppressWarnings({ suppressMessages({
    xi <- interpolate(x, mg, debug.level=0)
  }) })
  class(xi)
  #plot(xi)

  #  Prepare fine-scale grid of densities
  xdf <- as.data.frame(x, xy=TRUE)
  z <- as.data.frame(xi) ; z %>% head
  names(z) <- 'z'
  xdf$layer <- z$z
  xdf %>% head
  mean(xdf$layer)

  # Now use fine-scale raster to find predicted density at each grids location
  dist <- function(a, b){
    dt <- data.table((df2$x-a)^2+(df2$y-b)^2)
    return(which.min(dt$V1))}
  df1 <- data.table(x=grids$x, y=grids$y) ; head(df1)
  df2 <- data.table(x=xdf$x, y=xdf$y) ; head(df2)
  results <- df1[, j = list(Closest =  dist(x, y)), by = 1:nrow(df1)]
  grids$d_mean <- xdf$layer[results$Closest]

  # Test plot
  #hist(grids$d_mean)
  #mean(grids$d_mean, na.rm=TRUE)
  if(toplot){
    plotKFS()
    points(x=grids$x, y=grids$y, cex=grids$d_mean*cex_scale)
  }

  return(grids$d_mean)
}

d_mean <- predict_surface(dsm_keep, sample_table, grids, cex_scale=10)
grids$d_mean <- d_mean
head(grids)

# Save results
save(grids, file='tests/fw/dsm-estimate.RData')
save(dsm_keep, file='tests/fw/dsm-model.RData')


################################################################################
################################################################################
################################################################################
################################################################################
# Attempt CV bootstrapping

B <- 1000

data_table %>% head
sample_table %>% head

data_bs <- data_table[complete.cases(data_table),] ; data_bs %>% nrow
sample_bs <- sample_table[complete.cases(sample_table),] ; sample_bs %>% nrow

bootstraps <- data.frame()
i=1
for(i in 1:B){
  message('=========================================\n BOOSTRAP ',i,'\n=========================================')

  #=============================================================================
  message('--- Preparing bootstrap dataset ...')
  (seg_bs <- sample(sample_bs$Sample.Label, size=nrow(sample_bs),replace=TRUE))
  data_bsi <- data.frame()
  sample_bsi <- data.frame()
  i=1
  for(i in 1:length(seg_bs)){
    (segi <- seg_bs[i])
    (sampli <- sample_bs %>% dplyr::filter(Sample.Label == segi))
    sampli$Sample.Label <- i
    sample_bsi <- rbind(sample_bsi, sampli)
    (siti <- data_bs %>% dplyr::filter(Sample.Label == segi))
    if(nrow(siti)>0){
      siti$Sample.Label <- i
      data_bsi <- rbind(data_bsi, siti)
    }
  }

  if(nrow(data_bsi)<=0){
    data_bsi <- data.frame(Sample.Label = NA, Effort = NA, x = NA, y = NA, datetime = NA,
                           year = NA, day = NA, block = NA, z = NA, zmin = NA, zmax = NA, zsd = NA, zrange = NA, Region.Label = NA, doy = NA)
    (data_bsi <- data_bsi[0,])
  }
  data_bsi

  #=============================================================================
  message('--- Fitting detection function ...')
  suppressWarnings({suppressMessages({
    dso_bs <- Distance::ds(data = data_bs, truncation = 2.0, formula= ~1, key = 'hn', quiet=FALSE)
    #plot(dso_bs)
  }) })

  #=============================================================================
  message('--- Fitting spatial density model ...')
  suppressWarnings({
    #dsm_keep$formula
    dsm_bs <- dsm(dsm_keep$formula,
                  dso_bs, sample_bsi, data_bsi, family=tw(), method="REML")  })

  #=============================================================================
  # Predict on samples

  message('--- Predicting density surface ...')
  d_mean <- predict_surface(dsm_bs, sample_bsi, grids, toplot=TRUE, cex_scale = 4)
  d_mean %>% length
  nrow(grids)
  bs_resulti <- data.frame(grid_id = grids$id, iteration = i, month = 0, D = d_mean)
  head(bs_resulti)

  #=============================================================================
  # Saving result

  bootstraps <- rbind(bootstraps, bs_resulti)
  message('')
  save(bootstraps, file='tests/fw/dsm-bootstraps.RData')
}

################################################################################

nrow(bootstraps)

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




