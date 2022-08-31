################################################################################
# Set up grid
################################################################################
# Make base grid

grids <- make_grid(xlims = c(-129.75, -128.5),
                   ylims = c(52.75, 54.1),
                   grid_int = 1.287)

plot(x=grids$x, y=grids$y, cex=.1, pch=16)
range(grids$km2)
mean(grids$km2)

################################################################################
# Get seafloor features

library(readr)
library(bangarang)

data(kfs_seafloor)
seafloor <- kfs_seafloor
head(seafloor)

nrow(grids)
pb <- txtProgressBar(1, nrow(grids), style=3) # setup progress bar
i=1000
newgrids <- data.frame()
for(i in 1:nrow(grids)){
  (gridi <- grids[i,])
  (z <- get_seafloor(lon = gridi$x, lat = gridi$y, seafloor))
  z$zrange <- z$zmax - z$zmin
  gridi <- data.frame(gridi,z)
  newgrids <- rbind(newgrids, gridi)
  setTxtProgressBar(pb, i) # update progress bar
}
newgrids %>% head()
newgrids %>% tail()
newgrids$z %>% hist
plot(x=newgrids$x, y=newgrids$y, cex=(newgrids$z/400), pch=16)

################################################################################
# Assign into blocks

data(kfs_blocks_bbox)
blox <- kfs_blocks_bbox

x <- newgrids$x
y <- newgrids$y
toplot <- TRUE
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
newgrids$blocksub <- inblock
newgrids$blocksub %>% table
newgrids$block <- NA
newgrids$block[grep('CAA',inblock)] <- 'CAA'
newgrids$block[grep('SQU',inblock)] <- 'SQU'
newgrids$block[grep('CMP',inblock)] <- 'CMP'
newgrids$block[grep('EST',inblock)] <- 'EST'
newgrids$block[grep('WHA',inblock)] <- 'WHA'
newgrids$block[grep('WRI',inblock)] <- 'WRI'
newgrids$block[grep('VER',inblock)] <- 'VER'
newgrids$block[grep('MCK',inblock)] <- 'MCK'
newgrids$block %>% table(useNA='ifany')

################################################################################
# Final formatting

# Determine which points are in the KFS proper
data(kfs_boundary)
kfs <- kfs_boundary
data(kfs_land)
land <- kfs_land

mr <- data.frame(x = newgrids$x, y = newgrids$y)
newgrids$inkfs <- inKFS(mr,kfs,toplot=TRUE)$inkfs

newgrids$inwater <- inwater(mr,land,toplot=TRUE)$valid

gridfinal <-
  newgrids %>%
  dplyr::filter(inkfs == TRUE,
                inwater == TRUE,
                !is.na(block),
                !is.na(z)) %>%
  dplyr::select(y1:zrange, block)

head(gridfinal)
nrow(gridfinal)

plotKFS() ; blocki <- 'CAA'
gridi <- gridfinal[gridfinal$block == blocki,] ; points(x=gridi$x, y=gridi$y, cex=.5)
gridi$z %>% range

plotKFS() ; points(x=gridfinal$x, y=gridfinal$y, cex=.5)

nrow(gridfinal)

################################################################################
# Save

write.csv(gridfinal,file='data-raw/grid-kfs-1km.csv',quote=FALSE, row.names=FALSE)

# Include spatial grid as data object
grid_kfs <- read.csv('data-raw/grid-kfs-1km.csv',stringsAsFactors=FALSE)
usethis::use_data(grid_kfs, overwrite = TRUE)

