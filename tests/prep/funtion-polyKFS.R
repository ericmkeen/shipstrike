#########################################################
#########################################################
# fwKFS  | Eric M. Keen, v. July 2020
#########################################################
# function polyKFS

###################################################
###################################################


polyKFS <- function(toplot=FALSE){
  library(rgdal)
  library(sp)
  library(raster)
  library(rgeos)
  library(PBSmapping)
  data(nepacLLhigh)
  ###################################################

  kfs <- readOGR(
    dsn= "tests/kfs_shapefile/layers/",
    layer="POLYGON",
    verbose=FALSE
  )
  proj4string(kfs) <-CRS("+proj=utm +zone=10+datum=WGS84")


  #########################################################
  data("nepacLLhigh")
  nepac <- nepacLLhigh
  head(nepac)
  pids <- unique(nepac$PID) ; pids
  plist <- list()
  i=1
  for(i in 1:length(pids)){
    pidi <- pids[i] ; pidi
    mri <- nepac[nepac$PID==pidi,] ; head(mri)
    c1 = cbind(mri$X, mri$Y)
    #r1 = rbind(c1, c1[1, ])  # join
    P1 = Polygon(c1)
    Ps1 = Polygons(list(P1), ID = pidi)
    plist[[i]] <- Ps1
  }
  length(plist)

  SPs = SpatialPolygons(plist)
  SPs
  proj4string(SPs) <-CRS("+proj=utm +zone=10+datum=WGS84")

  #########################################################
  ?gIntersection
  kfsland <- gIntersection(SPs, kfs, byid = F)


  #########################################################
  if(toplot){
    plotKFS(area="Other",X=c(-130.7,-127.75),Y=c(52.5,54.1))
    plot(kfs,add=T)
    plot(kfsland,add=T)
  }

  return(list(kfs=kfs,land=kfsland))

}


#########################################################
#########################################################
# Points in KFS

#kpolys <- polyKFS()
#kfs <- kpolys$kfs
#land <- kpolys$land

#mr <- read.csv("../data/surveys.csv",stringsAsFactors=FALSE)
#head(mr)
#mri <- mr
#x <- mr$X ; y <- mr$Y

inKFS <- function(mri,kfs,toplot=FALSE){
  mri <- mri[!is.na(mri$X) & !is.na(mri$Y),] ; nrow(mri)
  x <- mri$X
  y <- mri$Y

  if(toplot){
    plotKFS(area="Other",X=c(-130.7,-127.75),Y=c(52.5,54.1))
    points(x,y,pch=16,cex=.4,col="steelblue3")
  }

  coords = cbind(as.numeric(as.character(x)),as.numeric(as.character(y))) ; coords
  sp = SpatialPoints(coords)
  proj4string(sp) <-CRS("+proj=utm +zone=10+datum=WGS84")

  otest <- over( sp , kfs , fn = NULL,returnList=FALSE)
  mri$inkfs <- TRUE
  mri$inkfs[is.na(otest)] <- FALSE
  table(mri$inkfs)
  mrin <- mri[mri$inkfs,] ; nrow(mrin)

  if(toplot){
    points(mrin$X,mrin$Y,pch=16,cex=.5,col="firebrick")
  }

  return(mri)
}

#inKFS(mr,kfs,toplot=TRUE)

#########################################################
#########################################################
# Points in the water?

#mr <- read.csv("../data/surveys.csv",stringsAsFactors=FALSE)
#head(mr)
#mri <- mr

inwater <- function(mri,land,toplot=FALSE){
  mri <- mri[!is.na(mri$X) & !is.na(mri$Y),] ; nrow(mri)
  x <- mri$X
  y <- mri$Y

  if(toplot){
    plotKFS(area="Other",X=c(-130.7,-127.75),Y=c(52.5,54.1))
    points(x,y,pch=16,cex=.4,col="steelblue3")
  }

  coords = cbind(as.numeric(as.character(x)),as.numeric(as.character(y))) ; coords
  sp = SpatialPoints(coords)
  proj4string(sp) <-CRS("+proj=utm +zone=10+datum=WGS84")

  otest <- over( sp , land , fn = NULL,returnList=FALSE)
  mri$valid <- FALSE
  mri$valid[is.na(otest)] <- TRUE
  table(mri$valid)
  mrin <- mri[mri$valid,] ; nrow(mrin)

  if(toplot){
    points(mrin$X,mrin$Y,pch=16,cex=.5,col="firebrick")
  }

  return(mri)
}

#inwater(mr,land)





