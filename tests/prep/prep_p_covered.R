# p_covered
# proportion of grid cells actually transited.
# scale number of grid cells selected accordingly

# Actual KM traveled
# When a ship crosses a 1-km2 grid, how much ground does it actually cover?
x <- 1:1000
y <- 1:1000
kms <- c()
for(i in 1:10000){
  (x1 <- sample(x,1))
  (x2 <- sample(x,1))
  (y1 <- sample(y,1))
  (y2 <- sample(y,1))
  (km <- sqrt(abs(x2 - x1)^2 + abs(y2 - y1)^2) / 1000)
  kms <- c(kms, km)
}
kms %>% hist

saveRDS(kms, file='tests/p_covered.RData')

