################################################################################
################################################################################
# Prep seasonal regression
################################################################################
################################################################################

library(dplyr)
library(lubridate)
library(ggplot2)

load('tests/hw/dsm-bootstraps.RData')

table(bootstraps$grid_id)
table(bootstraps$month)

boots <-
  bootstraps %>%
  dplyr::filter(month != 0) %>%
  dplyr::select(month, grid_id, D)

table(boots$grid_id)
table(boots$month)

head(boots)

################################################################################
# Extrapolate other months
df <- boots

df10 <- df %>% dplyr::filter(month==9)
df10$month <- 10
df10$D <- df10$D * 0.2

df11 <- df10
df11$month <- 11
df11$D <- df10$D * 0.2

df12 <- df11
df12$month <- 12
df12$D <- df11$D * 0.2

df1 <- df12
df1$month <- 1
df1$D <- df12$D * 0.2

df5 <- df %>% dplyr::filter(month==6)
df5$month <- 5
df5$D <- df5$D * 0.2

df4 <- df5
df4$month <- 4
df4$D <- df5$D * 0.2

df3 <- df4
df3$month <- 3
df3$D <- df4$D * 0.2

df2 <- df4
df2$month <- 2
df2$D <- sapply(1:nrow(df2), function(x){mean(c(df3$D[x], df1$D[x]))})

df <- rbind(df1, df2, df3, df4, df5, df, df10, df11, df12)

head(df)
nrow(df)

#df <-
#  df %>%
#  group_by(month, grid_id) %>%
#  summarize(D = D[sample(1:n(),size=1000, replace=FALSE)])

#df %>% nrow

saveRDS(df,file='tests/hw/pwhale_seasonal_boots.RData')

################################################################################
# To visuaslize

# Deal with forgetting to do iteration count properly
mr <- data.frame()
(months <- unique(df$month))
mi = 1
for(mi in 1:length(months)){
  (monthi <- months[mi])
  print(monthi)
  booti <- df %>% dplyr::filter(month == monthi)
  nrow(booti)
  head(booti)

  #(booti$grid_id %>% table %>% table)
  (iterations <- 5000)
  bootii <-
    booti %>%
    group_by(grid_id) %>%
    mutate(iteration = 1:iterations)

  bootiii <-
    bootii %>%
    group_by(iteration) %>%
    summarize(D = mean(D)) %>%
    mutate(month = monthi) %>%
    dplyr::select(month, iteration, D)

  bootiii %>% head
  # hist(bootiii$D)
  mr <- rbind(mr, bootiii)
}

mr %>% nrow
mr %>% head

ggplot(mr, aes(x=factor(month), y=D)) +
  geom_violin()

mrs <-
  mr %>%
  group_by(month) %>%
  summarize(q.50 = median(D),
            q.2.5 = quantile(D, .025),
            q.5 = quantile(D, .05),
            q.10 = quantile(D, .10),
            q.20 = quantile(D, .20),
            q.80 = quantile(D, .80),
            q.90 = quantile(D, .90),
            q.95 = quantile(D, .95),
            q.975 = quantile(D, .975))

#mrs
#ggplot(mrs, aes(x = month, y =  mn)) +
#  geom_ribbon(data=mrs, mapping=aes(x = month, ymin = lci, ymax = uci), alpha=.5) +
#  geom_line()

d2.5 <- data.frame(month = c(mrs$month, rev(mrs$month)),
                   q.50 = c(mrs$q.2.5, rev(mrs$q.975)))
d5 <- data.frame(month = c(mrs$month, rev(mrs$month)),
                 q.50 = c(mrs$q.5, rev(mrs$q.95)))
d10 <- data.frame(month = c(mrs$month, rev(mrs$month)),
                  q.50 = c(mrs$q.10, rev(mrs$q.90)))
d20 <- data.frame(month = c(mrs$month, rev(mrs$month)),
                  q.50 = c(mrs$q.20, rev(mrs$q.80)))

ggplot(mrs, aes(x=month, y=q.50)) +
  geom_polygon(data=d2.5, mapping=aes(x=month, y=q.50), fill='grey40',alpha=.1) +
  geom_polygon(data=d5, mapping=aes(x=month, y=q.50), fill='grey30',alpha=.1) +
  geom_polygon(data=d10, mapping=aes(x=month, y=q.50), fill='grey20',alpha=.1) +
  geom_polygon(data=d20, mapping=aes(x=month, y=q.50), fill='grey10',alpha=.1) +
  geom_line(lwd=1.25, alpha=1.0) +
  geom_abline(slope=0, intercept=1, lty=3, color='steelblue3') +
  scale_x_continuous(breaks=1:12, labels=months) +
  xlab(NULL) +
  ylab('Whale density') +
  theme_light()






