#########################################################
#########################################################
# fwKFS  | Eric M. Keen, v. July 2020
#########################################################
# point in kfs polygon

###################################################
###################################################
# Build up simple blocks

torun <- FALSE
if(torun){
  plotKFS()
  bloxim <- data.frame()
  uid <- unique(blox$block) ; uid
  i=1
  for(i in 1:length(uid)){
    idi <- uid[i]
    bloxi <- blox[blox$block==idi,]
    c1 = cbind(bloxi$X, bloxi$Y)
    P1 = Polygon(c1)
    Ps1 = Polygons(list(P1), ID = "a")
    SPs = SpatialPolygons(list(Ps1))
    bb <- bbox(SPs) ; bb
    px <- c(bb[1,1],bb[1,1],bb[1,2],bb[1,2],bb[1,1]) ; px
    py <- c(bb[2,1],bb[2,2],bb[2,2],bb[2,1],bb[2,1]) ; py
    lines(px,py)
    dfi <- data.frame(id=idi,
                      left=bb[1,1],right=bb[1,2],
                      bottom=bb[2,1],top=bb[2,2])
    dfi
    bloxim <- rbind(bloxim,dfi)
    
  }
  bloxim
  
  write.csv(bloxim,"../data/blocks.csv",row.names=FALSE,quote=FALSE)
}

###################################################
###################################################

# block="SQUS"
 #x <- -129.38
 #y <- 53.18
 #x <- mr$X
 #y <- mr$Y



pip.kfs <- function(x,y,toplot=FALSE){
  #library(rgdal)
  library(sp)
  #library(raster)
  #library(rgeos)
  #library(bangarang)
  
  blox <- read.csv("../data/blocks.csv") ; blox
  inblock <- rep("",times=length(x))
  i=11
  for(i in 1:nrow(blox)){
    bloxi <- blox[i,] ; bloxi
    px <- c(bloxi$left,bloxi$left,bloxi$right,bloxi$right,bloxi$left)
    py <- c(bloxi$bottom,bloxi$top,bloxi$top,bloxi$bottom,bloxi$bottom)
    
    if(toplot){
      plotKFS()
      points(x,y,col="grey",cex=.4,pch=16)
      lines(px,py)
    }
    
    pipresult <- sp::point.in.polygon(x,y,px,py)
    ins <- which(pipresult>0)
    
    if(toplot){
      points(x[ins],y[ins],col="firebrick",cex=.6,pch=16)
    }
    
    inblock[ins] <- paste0(inblock[ins],"-",bloxi$id)
    
  }
  inblock
  return(inblock)
  
}



