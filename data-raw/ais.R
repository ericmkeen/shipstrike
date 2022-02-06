# AIS
ais <- read.csv("data-raw/ais-2018.csv")

usethis::use_data(ais, overwrite = TRUE)
