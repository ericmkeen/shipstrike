###################################################
###################################################
# Fin Distance Simulator: GRID MAKER
###################################################
###################################################
setwd("/Users/ekeen/Google Drive/Open/fin")
source("functions-maps.R")

###################################################
# Base map
library(bangarang)
plotKFS(area="Other",X=c(-129.6,-129.3),Y=c(53.1,53.35))

###################################################
# Build grid

int <- .001
xs <- seq(-129.52,-129.345,by=int)
ys <- seq(53.12,53.34,by=int)

MR <- data.frame()
i=2
for(i in 2:length(ys)){
  print(paste0("Building grid :: ",round(100*(i/length(ys)),digits=2),"% complete...."))
  j=2
  for(j in 2:length(xs)){
    left <- xs[j-1] ; left
    right <- xs[j] ; right
    bottom <- ys[i-1] ; bottom
    top <- ys[i] ; top
    X <- mean(c(left,right)) ; X
    Y <- mean(c(bottom,top)) ; Y
    mri <- data.frame(X,Y,left,right,bottom,top) ; mri
    MR <- rbind(MR,mri)
  }
}

points(MR$X,MR$Y,pch=16,cex=.2)
nrow(MR)

###################################################
# Determine whether in polygon

coord <- data.frame(longitude=MR$X,latitude=MR$Y)
sp = SpatialPoints(coord) ; class(sp)

shoredata <- nepacLLhigh
studyarea <- shoredata[which(shoredata$X >= -129.7 & shoredata$X <= -129.3 &
                               shoredata$Y > 53.1 & shoredata$Y < 53.4),]
pids <- unique(studyarea$PID)[2:20] ; pids
shore <- shoredata[shoredata$PID %in% pids,] ; nrow(shore) ; head(shore)

keep <- data.frame(ID=1:nrow(MR),keep=1) ; keep

i=1
for(i in 1:length(pids)){
  shori <- shore[shore$PID==pids[i],] ; nrow(shori)
  points(shori$X,shori$Y,pch=16,cex=.4)
  c1 = cbind(shori$X, shori$Y)
  r1 = rbind(c1, c1[1, ])  # join
  P1 = Polygon(r1)
  Ps1 = Polygons(list(P1), ID = "a")
  shori = SpatialPolygons(list(Ps1))
  #shori <- as(SPs, "Spatial") ; class(shori)
  
  inpoly <- sp::over(sp,shori) ; inpoly
  inpoly <- which(!is.na(inpoly)) ; inpoly
  keep$keep[inpoly] <- 0
  
  #nrow(MR)
  #MRS <- MR[inpoly,] ; nrow(MRS)
  #ID <- 1:nrow(MRS)
  #MRS <- data.frame(ID,MRS)
}

MR <- MR[keep$keep==1,]

# Add good points polygons
points(MR$X,MR$Y,pch=16,cex=.2,col="firebrick")

###################################################
###################################################
# Calculate distance from Fin Island cabin to each point

library(swfscMisc)
fin.cabin <- c(-129.3720833,53.23690)

km <- brng <- rep(NA,times=nrow(MR)) ; i=1
for(i in 1:nrow(MR)){
  print(paste0("Getting distance to fin :: ",round(100*(i/length(km)),digits=2),"% complete...."))
  x <- MR$X[i] ; x
  y <- MR$Y[i] ; y
  
  # distance to point
  nmi <- distance(lat1=y,lon1=x,lat2=fin.cabin[2],lon2=fin.cabin[1],method="vincenty",units="nm")   ; nmi
  kmi <- nmi * 1.852 ; kmi
  km[i] <- kmi
  
  # bearing to point from fin
  brngi <- as.numeric(bearing(lat1=fin.cabin[2],lon1=fin.cabin[1],lat2=y,lon2=x)[1]) ; brngi
  brng[i] <- brngi
}
hist(km)
hist(brng)

MR$km <- km
MR$brng <- brng
MR$ID <- 1:nrow(MR)
head(MR)

###################################################
###################################################
# Write to file
setwd("/Users/ekeen/Google Drive/Open/fin")
getwd()
write.csv(MR,file="./distsim/fin-grid-001.csv",quote=FALSE,row.names=FALSE)

