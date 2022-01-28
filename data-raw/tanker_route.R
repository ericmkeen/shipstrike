
library(bangarang)
data(shiplane) # bangarang package
tanker1 <- shiplane %>% dplyr::filter(PID==1, POS < 51)
tanker2 <- shiplane %>% dplyr::filter(PID==3, POS < 51)
(tanker_route <- rbind(tanker1, tanker2) %>% select(x=X, y=Y))

usethis::use_data(tanker_route, overwrite = TRUE)
