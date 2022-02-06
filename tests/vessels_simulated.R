################################################################################
# vessels (simulated)
################################################################################
################################################################################
# Make grid

grids <- read.csv('tests/grid-kfs-1km.csv',stringsAsFactors=FALSE)

#grids <- make_grid(xlims = c(-129.75, -128.5),
#                   ylims = c(52.75, 54.1),
#                   grid_int = 1.287)

plot(x=grids$x, y=grids$y, cex=.1, pch=16)
range(grids$km2)
mean(grids$km2)

library(bangarang)
plotKFS()
points(x=grids$x, y=grids$y, cex=.2, pch=16)

################################################################################
# Simulate vessels

data(tanker_route, package='shipstrike') # shipstrike package
tanker <- list(type='tanker',
               n=100,
               size_min = 200,
               size_max = 350,
               speed_min = 10,
               speed_max = 12,
               width = 0.15,
               draft = 0.1,
               hour = NULL,
               months = 1:12,
               direction = 0, # 0 = reg; 1 = rev; 2 = random
               course_var = .002,
               course = tanker_route)

vessels <- simulate_vessel(grids, vessels=list(tanker))
vessels %>% head

#plotKFS()
#points(x=vessels$x,y=vessels$y, col='red', pch=16, cex=.5)

################################################################################
# Interpolate & grid associate vessels
# (this is where you would bring in actual AIS observations)

vgrid <- vessel_grid(grids, vessels)
head(vgrid)
nrow(vgrid)

################################################################################
# Determine sun positions

vgrid$sun <- vessel_sun_angle(vgrid)
vgrid$diel <- 'night'
vgrid$diel[vgrid$sun > 0] <- 'day'
vgrid$diel %>% table

################################################################################
# Summarize grid cells by type, diel, month

vsumm <- summarize_grid(vgrid)
vsumm %>% filter(type == 'tanker') %>% head
nrow(vsumm)

# Speed, length, widht, and draft are saved as collapsed vectors:
(speeds <- stringr::str_split(vsumm$speed[1], pattern='_'))[[1]] %>% as.numeric
(speeds <- stringr::str_split(vsumm$speed[nrow(vsumm)], pattern='_'))[[1]] %>% as.numeric

################################################################################
# Write data
vessels_sim <- vsumm
save(vessels_sim, file='tests/vessels_sim.RData')



