# Speed - mortality curves

fitlog <- function(v1, p1, v2, p2, asymptote = 0.8){
  #v1 = 4.6; p1 = 0.5; v2 = 13, p2 = 0.91
  c1 <- seq(-10, 0, length=1000)
  c2 <- seq(0.001, 10, length = 1000)
  df <- expand.grid(c1, c2)
  df$p1 <- (asymptote/ (1 + exp(-1*(df$Var1 + (df$Var2*v1)))))
  df$e1 <- p1 - df$p1
  df$p2 <- (asymptote/ (1 + exp(-1*(df$Var1 + (df$Var2*v2)))))
  df$e2 <- p2 - df$p2
  df$etot <- abs(df$e1) + abs(df$e2)
  mine <- which.min(df$etot)
  hist(df$etot)
  df[mine,]

  speeds <- seq(0, 40, length=1000)
  (c1 <- df$Var1[mine])
  (c2 <- df$Var2[mine])
  morts <- (asymptote/ (1 + exp(-1*(c1 + (c2*speeds)))))
  plot(morts~speeds, type='l', col='firebrick', ylim=c(0,1), lwd=2)
  abline(h=p1, v=v1, lty=1, col='steelblue3')
  abline(h=p2, v=v2, lty=1, col='steelblue3')

  dfmin <- data.frame(c1, c2, asymptote)
  return(dfmin)
}

#"Other > 40m"         "Passenger > 100m"    "Tug < 50m"           "Towing < 50m"        "Cargo > 100m"
#"Fishing < 60m"       "Other < 40m"         "Pleasurecraft < 40m" "Sailing"             "Tanker > 100m"

asymptote <- 0.9

(df1 <-
    data.frame(type = 'Pleasurecraft < 40m | Other < 40m | Sailing',
               fitlog(v1 = 16, p1 = 0.5*asymptote,
                      v2 = 22, p2 = 0.8*asymptote, asymptote = asymptote)))

asymptote <- 1

(df2 <-
    data.frame(type = 'Tug < 50m | Towing < 50m',
               #tonnage = 15,  # 18m yacht
               fitlog(v1 = 10, p1 = 0.5*asymptote,
                      v2 = 16, p2 = 0.8*asymptote, asymptote = asymptote)))

(df3 <-
    data.frame(type = 'Fishing < 60m | Other > 40m',
               #tonnage = 45, # Atlantic Cod CFV in Kelley et al
               fitlog(v1 = 6.6, p1 = 0.5*asymptote,
                      v2 = 13 , p2 = 0.795*asymptote, asymptote = asymptote)))

(df4 <-
    data.frame(type = 'Cargo > 180m | Passenger > 180m | Tanker > 180m | Other > 100m',
               #tonnage = "10,000 - 30,000",
               fitlog(v1 = 4.6, p1 = 0.5*asymptote,
                      v2 = 13 , p2 = 0.91*asymptote, asymptote = asymptote)))

(dfs <- rbind(df1, df2, df3, df4))

saveRDS(dfs, file='data-raw/p_lethal.RData')
p_lethality <- readRDS('data-raw/p_lethal.RData')
usethis::use_data(p_lethality, overwrite = TRUE)


# Plot

predlog <- function(speeds = seq(0, 30, length=1000),
                    c1, c2, asymptote=0.8){
  morts <- (asymptote/ (1 + exp(-1*(c1 + (c2*speeds)))))
  dfi <- data.frame(speeds, morts)
  return(dfi)
}

mr <- data.frame()
i=4
for(i in 1:nrow(dfs)){
  (dfi <- dfs[i,])
  preds <- predlog(c1=dfi$c1, c2 = dfi$c2, asymptote = dfi$asymptote)
  preds$type = dfi$type
  mr <- rbind(mr, preds)
}

saveRDS(mr, file='data-raw/lethal_fig.RData')

ggplot(mr, aes(x=speeds, y=morts, color=type)) +
  geom_line(lwd=1, alpha=.8) +
  scale_x_continuous(breaks=seq(0,30,by=5)) +
  scale_y_continuous(breaks=seq(0,1,by=.1), limits=c(0,1)) +
  theme_light() +
  xlab('Vessel speed') +
  ylab('P(Lethal)') +
  labs(color = 'Vessel class group')

