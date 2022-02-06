# Include spatial grid as data object

grid_kfs <- read.csv('tests/grid-kfs-1km.csv',stringsAsFactors=FALSE)

usethis::use_data(grid_kfs, overwrite = TRUE)


