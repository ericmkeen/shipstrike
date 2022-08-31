# Tanker sim

data(tanker_route)

# Number of transits expected July 1 - Oct 31, 2030 (4 months)
# based on 750 new tankers (1500 transits per year)
(new_transits <- 1500 * (4/12))

# Prepare details for simulating tanker traffic
tankers <- list(type='tanker',
                n=new_transits,
                size_min = 270,
                size_max = 315,
                speed_min = 8,
                speed_max = 14,
                width = 0.15, # beam / length
                draft = 0.05, # draft / length
                course_var = .002, # add v. minor course variability
                course = tanker_route)

vessels <- simulate_vessel(grids,
                           vessels=list(tankers),
                           toplot=FALSE,
                           verbose=FALSE)

vgrid_sim <- vessel_grid(grid_kfs, vessels, verbose=FALSE)

vgrid_sim$sun <- vessel_sun_angle(vgrid_sim, verbose=FALSE)

vgrid_sim$diel <- 'night'
vgrid_sim$diel[vgrid_sim$sun > 0] <- 'day'

head(vgrid_sim)
tankers2030 <- vgrid_sim
usethis::use_data(tankers2030, overwrite = TRUE)

