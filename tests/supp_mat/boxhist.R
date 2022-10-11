

library(ggplot2)

height_scaling <- .95
fill_color <- 'darkslategrey'
bin_range <- c(-0.5, 10.5)
bin_int <- 1
x <- rnorm(1000, mean = 5, sd=1)
y <- 2


# Get histogram counts
bin_breaks <- seq(min(bin_range), max(bin_range), by=bin_int)
counts <- hist(x, breaks=bin_breaks, plot = FALSE)$counts
length(counts)
length(bin_breaks)
(bind_mid <- bin_breaks + (bin_int/2))

# Build dataframe of polygon instructions
(mr <-
  data.frame(left = bin_breaks[1:(length(bin_breaks)-1)],
             right = bin_breaks[2:length(bin_breaks)],
             counts = counts) %>%
  mutate(height_raw = counts / max(counts)) %>%
  mutate(height = height_raw * height_scaling) %>%
  mutate(top = y + height,
         bottom = y - height))

# Pivot into a poylgon df for plotting
polys <- data.frame()
i=5
for(i in 1:nrow(mr)){
  (mri <- mr[i,])
  (xi <- c(mri$left, mri$right, mri$right, mri$left, mri$left))
  (yi <- c(mri$bottom, mri$bottom, mri$top, mri$top, mri$bottom))
  poli <- data.frame(i, xi, yi)
  polys <- rbind(polys, poli)
}
polys

# Plot
ggplot() + geom_polygon(data = polys,
                        mapping=aes(x=xi, y=yi, group=i),
                        color=NA,
                        fill=fill_color,
                        alpha = .8)




