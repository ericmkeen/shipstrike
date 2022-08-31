
library(bangarang)
data(shiplane) # bangarang package
tanker1 <- shiplane %>% dplyr::filter(PID==1, POS < 51)
tail(tanker1)
tanker2 <- shiplane %>% dplyr::filter(PID==3, POS < 51)
head(tanker2)

tanker_bw <- data.frame(PID = NA, POS = NA, X = seq(-129.3755, -129.5634, length=10),
                        Y = seq(53.20965, 53.19266, length = 10))

(tanker_route <- rbind(tanker1, tanker_bw, tanker2) %>% select(x=X, y=Y))

tanker_route

i=1
tanker_route2 <- data.frame()
for(i in 1:nrow(tanker_route)){
  (xy1 <- tanker_route[i,])
  (xy2 <- tanker_route[min(c(nrow(tanker_route), (i+1))),])
  (xy_int <- data.frame(x = seq(xy1$x, xy2$x, length = 11),
             y = seq(xy1$y, xy2$y, length = 11)) %>% head(10))
  tanker_route2 <- rbind(tanker_route2, xy_int)
}

tanker_route2

plotKFS()
points(x=tanker1$X, y=tanker1$Y, col='red')
points(x=tanker2$X, y=tanker2$Y, col='blue')
points(x=tanker_route$x, y=tanker_route$y)
points(x=tanker_route2$x, y=tanker_route2$y, cex=.2)

tanker_route <- tanker_route2
usethis::use_data(tanker_route, overwrite = TRUE)
