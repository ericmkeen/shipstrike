# Actual data processing is in...
# Dropbox > NCCS > Projects > ship-strike > data > tags

# Some processing is also in
# shipstrike > tests > psurface

p_surface <- readRDS('data-raw/p_surface.RData')
usethis::use_data(p_surface, overwrite = TRUE)
