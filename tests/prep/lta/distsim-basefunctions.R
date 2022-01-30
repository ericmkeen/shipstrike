###################################################
###################################################
# DISTANCE SIMULATOR: BASE FUNCTIONS
###################################################
###################################################
###################################################
###################################################
# Distribution simulator 

#d1 <- c(53.18,.015,-129.35,.015) # lat mean, lat sd, lon mean, lon sd
#d2 <- c(53.31,.015,-129.48,.015) # NULL
#d3 <- NULL


whalesim.prior <- function(n,showproc=FALSE){
  
  # Load grid
  #setwd("/Users/erickeen/Desktop/distsim-office")
  #setwd("/Users/ekeen/Google Drive/Open/fin")
  MR <- read.csv("./distsim/fin-surface.csv",stringsAsFactors=FALSE)
  head(MR)
  #hist(MR$km,col="grey50",border="grey80",breaks=20,xlab="KM from Fin",main="Grid locations")
  
  # Whale sampler
  sam <- sample(MR$ID,size=n,prob=MR$P,replace=TRUE) ; sam
  simwhales <- data.frame()
  for(i in 1:length(sam)){
    sami <- sam[i]
    matchi <- which(MR$ID==sami) ; matchi
    simwhales <- rbind(simwhales,MR[matchi,])
  }
  head(simwhales) ; nrow(simwhales)
  #simwhales <- MR[MR$ID%in%sam,]
  
  if(showproc){
    par(mar=c(0,0,0,0))
    par(mfrow=c(2,2))
    # Latitude probability
    #plotKFS(area="Other",X=c(-129.6,-129.3),Y=c(53.1,53.35))
    #points(MR$X,MR$Y,pch=16,cex=(.1*yp))
    
    # Longitude probability
    #plotKFS(area="Other",X=c(-129.6,-129.3),Y=c(53.1,53.35))
    #points(MR$X,MR$Y,pch=16,cex=(.1*xp))
    
    # Lat-Long probability
    plotKFS(area="Other",X=c(-129.6,-129.3),Y=c(53.1,53.35))
    polygon(x=c(-129.52,-129.345,-129.345,-129.52,-129.52),
            y=c(53.12,53.12,53.34,53.34,53.12),border="grey50")
    points(MR$X,MR$Y,pch=16,cex=(.7*MR$P))
    
    # Simulated whales
    plotKFS(area="Other",X=c(-129.6,-129.3),Y=c(53.1,53.35))
    polygon(x=c(-129.52,-129.345,-129.345,-129.52,-129.52),
            y=c(53.12,53.12,53.34,53.34,53.12),border="grey50")
    points(simwhales$X,simwhales$Y,pch=16,cex=.3)
    
    # Histograms: Distance
    par(mar=c(5,5,.5,.5))
    hist(simwhales$km,breaks=20,col="grey60",border="grey80",main=NULL,xlab="Kilometers from Fin",xlim=c(0,15))
    
    # Histograms: Bearing
    par(mar=c(5,5,.5,.5))
    hist(simwhales$brng,breaks=20,col="grey60",border="grey80",main=NULL,xlab="Bearing from Fin",xlim=c(90,360),at=c(90,120,150,180,210,240,270,300,330,360))
    
    par(mfrow=c(1,1))
  }
  
  return(simwhales)
}

###################################################
###################################################
###################################################

whalesim <- function(n,d1,d2=NULL,d3=NULL,showproc=FALSE){
  
  # Load grid
  setwd("/Users/erickeen/Google Drive/Open/fin")
  MR <- read.csv("./distsim/fin-grid-001.csv",stringsAsFactors=FALSE)
  head(MR)
  #hist(MR$km,col="grey50",border="grey80",breaks=20,xlab="KM from Fin",main="Grid locations")
  
  # Get distributions
  xp1 <- xp2 <- xp3 <- rep(0.0001,times=nrow(MR))
  yp1 <- yp2 <- yp3 <- rep(0.0001,times=nrow(MR))
  P1 <- P2 <- P3 <- rep(0,times=nrow(MR))
  if(!is.null(d1)){
    yp1 <- dnorm(MR$Y,mean=d1[1],sd=d1[2])
    xp1 <- dnorm(MR$X,mean=d1[3],sd=d1[4])
    P1 <- yp1*xp1
    P1 <- P1/max(P1)
    #hist(P1)
  }
  if(!is.null(d2)){
    yp2 <- dnorm(MR$Y,mean=d2[1],sd=d2[2])
    xp2 <- dnorm(MR$X,mean=d2[3],sd=d2[4])
    P2 <- yp2*xp2
    P2 <- P2/max(P2)
    #hist(P2)
  }
  if(!is.null(d3)){
    yp3 <- dnorm(MR$Y,mean=d3[1],sd=d3[2])
    xp3 <- dnorm(MR$X,mean=d3[3],sd=d3[4])
    P3 <- yp3*xp3
    P3 <- P3/max(P3)
    #hist(P3)
  }
  
  # Combine probabilities
  xp <- xp1/max(xp1) + xp2/max(xp2) + xp3/max(xp3) ; #hist(xp)
  yp <- yp1/max(yp1) + yp2/max(yp2) + yp3/max(yp3) ; #hist(yp)
  P <- P1 + P2 + P3 ; #hist(P)
  
  # Whale sampler
  sam <- sample(MR$ID,size=n,prob=P,replace=TRUE) ; sam
  simwhales <- data.frame()
  for(i in 1:length(sam)){
    sami <- sam[i]
    matchi <- which(MR$ID==sami) ; matchi
    simwhales <- rbind(simwhales,MR[matchi,])
  }
  head(simwhales) ; nrow(simwhales)
  #simwhales <- MR[MR$ID%in%sam,]
  
  if(showproc){
    par(mar=c(0,0,0,0))
    par(mfrow=c(2,2))
    # Latitude probability
    #plotKFS(area="Other",X=c(-129.6,-129.3),Y=c(53.1,53.35))
    #points(MR$X,MR$Y,pch=16,cex=(.1*yp))
    
    # Longitude probability
    #plotKFS(area="Other",X=c(-129.6,-129.3),Y=c(53.1,53.35))
    #points(MR$X,MR$Y,pch=16,cex=(.1*xp))
    
    # Lat-Long probability
    plotKFS(area="Other",X=c(-129.6,-129.3),Y=c(53.1,53.35))
    polygon(x=c(-129.52,-129.345,-129.345,-129.52,-129.52),
            y=c(53.12,53.12,53.34,53.34,53.12),border="grey50")
    points(MR$X,MR$Y,pch=16,cex=(.3*P))
    
    # Simulated whales
    plotKFS(area="Other",X=c(-129.6,-129.3),Y=c(53.1,53.35))
    polygon(x=c(-129.52,-129.345,-129.345,-129.52,-129.52),
            y=c(53.12,53.12,53.34,53.34,53.12),border="grey50")
    points(simwhales$X,simwhales$Y,pch=16,cex=.25)
    
    # Histograms: Distance
    par(mar=c(5,5,.5,.5))
    hist(simwhales$km,breaks=20,col="grey60",border="grey80",main=NULL,xlab="Kilometers from Fin",xlim=c(0,15))
    
    # Histograms: Bearing
    par(mar=c(5,5,.5,.5))
    hist(simwhales$brng,breaks=20,col="grey60",border="grey80",main=NULL,xlab="Bearing from Fin",xlim=c(90,360),at=c(90,120,150,180,210,240,270,300,330,360))
    
    par(mfrow=c(1,1))
  }
  
  return(simwhales)
}

# Random
#mr <- whalesim(n=500,
#               d1=c(53.18,3,-129.37,20),
#               showproc=TRUE)

# Hotspots near Gil, Otter, and Twartz
#mr <- whalesim(n=500,
#               d1=c(53.18,.025,-129.37,.025),
#               d2=c(53.28,.025,-129.46,.025),
#               d3=c(53.2,.025,-129.48,.025),
#               showproc=TRUE)
#head(mr)

###################################################
###################################################
###################################################
###################################################
###################################################
###################################################
# Parse distance curve by bearing windows

#dat <- mr
#b1 <- NULL #c(0,180)
#b2 <- NULL #c(180,270)
#b3 <- NULL #c(270,360)
#b4 <- NULL
#b5 <- NULL

bearing.parse <- function(dat,b1=NULL,b2=NULL,b3=NULL,b4=NULL,b5=NULL){
  
  alldat <- dat
  alldat$Bearing.Range <- "All"
  
  bclass <- rep(NA,times=nrow(alldat))
  if(!is.null(b1)){bclass[dat$brng >= b1[1] & dat$brng <= b1[2]] <- paste0(b1[1],"-",b1[2])}
  if(!is.null(b2)){bclass[dat$brng >= b2[1] & dat$brng <= b2[2]] <- paste0(b2[1],"-",b2[2])}
  if(!is.null(b3)){bclass[dat$brng >= b3[1] & dat$brng <= b3[2]] <- paste0(b3[1],"-",b3[2])}
  if(!is.null(b4)){bclass[dat$brng >= b4[1] & dat$brng <= b4[2]] <- paste0(b4[1],"-",b4[2])}
  if(!is.null(b5)){bclass[dat$brng >= b5[1] & dat$brng <= b5[2]] <- paste0(b5[1],"-",b5[2])}
  bclass
  dat$Bearing.Range <- bclass
  dat <- dat[!is.na(dat$Bearing.Range),] ; nrow(dat)
  
  # Add "All" category
  if(nrow(dat)>0){alldat <- rbind(alldat,dat)}
  
  # Distance histogram (partition by bearing)
  library(ggplot2)
  print(ggplot(alldat, aes(km, fill = Bearing.Range)) + geom_density(alpha = 0.2))
  
  #print(ggplot(alldat, aes(km, fill = bclass)) + 
  #  geom_histogram(alpha = 0.5, aes(y = ..density..), position = 'identity',bins=20))
  return(alldat)
}

#bearing.parse(dat=mr)
#bearing.parse(dat=mr,b1=c(0,180))
#bearing.parse(dat=mr,b1=c(0,180),b2=c(180,270),b3=c(270,360))


###################################################
###################################################
###################################################
###################################################
###################################################
###################################################
###################################################
# Apply a detection probability curve to a set of sightings

#actual=mr
#lambda=5
#p=2.5

library(fdrtool)
# Use param 2
simdetect <- function(actual,theta,showproc=FALSE){
  
  # Create distance probability function 
  x  = seq(0, 15, 0.01)

  Pexp <- dhalfnorm(x,theta=theta) ; Pexp   # USE HALF NORMAL APPROACH
  Pexp <- Pexp/max(Pexp,na.rm=TRUE)
  detfunk <- data.frame(km=x,pdet=Pexp,row.names=NULL) ; head(detfunk)
  #plot(detfunk$pdet~detfunk$km,type="l",lwd=3,col="firebrick")
  
  det.funk <- function(ai,DPF=detfunk){
    diffs <- ai[7] - DPF[,1]
    match <- which.min(abs(diffs)) ; #print(dpf[match,])
    probi <- DPF[match,2] ; probi
   
     # Based on that prob, test whether it was seen
    seeni <- sample(x=c(1,0),size=1,replace=FALSE,prob=c(probi,(1-probi))) ; seeni
    return(seeni)
  }
  
  seen <- apply(actual,1,det.funk) ; head(seen)
  
  actual$seen <- seen
  
  detected <- actual[actual$seen==1,] ; nrow(detected)
  
  # Stats
  metrics <- data.frame(n.actual=nrow(actual),
                        n.detect=nrow(detected),
                        p.seen=round(nrow(detected)/nrow(actual),digits=4))
  metrics
  
  if(showproc){
    par(mfrow=c(2,2))
    
    # Prob Den Function
    par(mar=c(5,5,3,.5))
    plot(detfunk[,2]~detfunk[,1],type="l",lty=1,lwd=3,
         xlab="KM from Fin",ylab="Probability of detection",
         main=paste0("theta = ",round(theta,digits=3)))
    
    # Plot result
    par(mar=c(0,0,0,0))
    plotKFS(area="Other",X=c(-129.6,-129.3),Y=c(53.1,53.35))
    polygon(x=c(-129.52,-129.345,-129.345,-129.52,-129.52),
            y=c(53.12,53.12,53.34,53.34,53.12),border="grey50")
    points(actual$X,actual$Y,pch=16,cex=.6,col="grey")
    points(detected$X,detected$Y,pch=16,cex=.5,col="black")
    
    # Histograms: Distance
    if(nrow(detected)>0){
    par(mar=c(5,5,.5,.5))
    hist(detected$km,breaks=20,col="grey60",border="grey80",main=NULL,xlab="KM from Fin",xlim=c(0,15))
    
    # Histograms: Bearing
    par(mar=c(5,5,.5,.5))
    hist(detected$brng,breaks=20,col="grey60",border="grey80",main=NULL,xlab="Bearing from Fin",xlim=c(90,360),at=c(90,120,150,180,210,240,270,300,330,360))
    }
    par(mfrow=c(1,1))
  }
  
  result <- list(metrics=metrics,actual=actual,detected=detected)
  return(result)
}

#
#result <- simdetect(actual=mr,lambda=5,p=2.5,showproc=TRUE) ; result$metrics
#result <- simdetect(actual=mr,lambda=8,p=2.5,showproc=TRUE) ; result$metrics
#detected <- result$detected
#bearing.parse(dat=result$detected,b1=c(0,180))
#bearing.parse(dat=result$detected,b1=c(0,180),b2=c(180,270),b3=c(270,360))

###################################################
###################################################
###################################################
###################################################
###################################################
###################################################
# Compare histogram of actual and detected

#head(actual)
#head(detected)
#int = .5
#lims=c(0,15)

km.compare <- function(actual,detected,int=.5,lims=c(0,15)){
  # Distance comparison
  starts <- seq(lims[1],lims[2],by=int) ; starts
  ends <- starts + int ; ends
  
  breakdown <- data.frame() ; i=1
  for(i in 1:length(starts)){
    si <- starts[i] ; si
    ei <- ends[i] ; ei
    n.there <- nrow(actual[actual$km >= si & actual$km < ei,]) ; n.there
    n.seen <- nrow(detected[detected$km >= si & detected$km < ei,]) ; n.seen
    minus <- n.there - n.seen
    prop <- n.seen / n.there
    bdi <- data.frame(km=si,int=int,n.there,n.seen,minus,prop)
    breakdown <- rbind(breakdown,bdi)
  }
  breakdown
  
  par(mfrow=c(2,2))
  # There vs Seen
  par(mar=c(5,5,3,1))
  barplot(height=breakdown$n.there,names.arg=breakdown$km,space=0,col=adjustcolor("steelblue3",alpha.f=.5),border=adjustcolor("steelblue",alpha.f=.7),xlab="KM from Fin",ylab="Frequency",main="# There vs # Seen")
  barplot(height=breakdown$n.seen,names.arg=breakdown$km,space=0,col=adjustcolor("darkorange",alpha.f=.5),border=adjustcolor("darkorange",alpha.f=.7),add=TRUE,ann=FALSE,axes=FALSE)
  
  # Diff
  par(mar=c(5,5,3,1))
  barplot(height=breakdown$minus,names.arg=breakdown$km,space=0,col=adjustcolor("black",alpha.f=.4),border=adjustcolor("grey80",alpha.f=.7),xlab="KM from Fin",ylab="Frequency",main="# There - # Seen")
  
  # Prop
  par(mar=c(5,5,3,1))
  barplot(height=breakdown$prop,names.arg=breakdown$km,space=0,col=adjustcolor("black",alpha.f=.4),border=adjustcolor("grey80",alpha.f=.7),xlab="KM from Fin",ylab="Frequency",main="# Seen / # There")
  
  par(mfrow=c(1,1))
}

#km.compare(actual=actual,detected=detected,int=.5,lims=c(0,15))

###################################################
###################################################

#int = 30
#lims=c(120,360)
brng.compare <- function(actual,detected,int=30,lims=c(120,360)){
  # Distance comparison
  starts <- seq(lims[1],lims[2],by=int) ; starts
  ends <- starts + int ; ends
  
  breakdown <- data.frame() ; i=1
  for(i in 1:length(starts)){
    si <- starts[i] ; si
    ei <- ends[i] ; ei
    n.there <- nrow(actual[actual$brng >= si & actual$brng < ei,]) ; n.there
    n.seen <- nrow(detected[detected$brng >= si & detected$brng < ei,]) ; n.seen
    minus <- n.there - n.seen
    prop <- n.seen / n.there
    bdi <- data.frame(brng=si,int=int,n.there,n.seen,minus,prop)
    breakdown <- rbind(breakdown,bdi)
  }
  breakdown
  
  par(mfrow=c(2,2))
  # There vs Seen
  par(mar=c(5,5,3,1))
  barplot(height=breakdown$n.there,names.arg=breakdown$brng,space=0,col=adjustcolor("steelblue3",alpha.f=.5),border=adjustcolor("steelblue",alpha.f=.7),xlab="Bearing from Fin",ylab="Frequency",main="# There vs # Seen")
  barplot(height=breakdown$n.seen,names.arg=breakdown$brng,space=0,col=adjustcolor("darkorange",alpha.f=.5),border=adjustcolor("darkorange",alpha.f=.7),add=TRUE,ann=FALSE,axes=FALSE)
  
  # Diff
  par(mar=c(5,5,3,1))
  barplot(height=breakdown$minus,names.arg=breakdown$brng,space=0,col=adjustcolor("black",alpha.f=.4),border=adjustcolor("grey80",alpha.f=.7),xlab="Bearing from Fin",ylab="Frequency",main="# There - # Seen")
  
  # Prop
  par(mar=c(5,5,3,1))
  barplot(height=breakdown$prop,names.arg=breakdown$brng,space=0,col=adjustcolor("black",alpha.f=.4),border=adjustcolor("grey80",alpha.f=.7),xlab="Bearing from Fin",ylab="Frequency",main="# Seen / # There")
  
  par(mfrow=c(1,1))
}

#brng.compare(actual,detected,int=30,lims=c(120,360))

###################################################
###################################################
###################################################
###################################################

#whales <- whalesim(n=100, d1=c(53.18,3,-129.37,20),showproc=TRUE) ; head(whales)
#detections <- simdetect(actual=whales,lambda=lambda,p=p,showproc=TRUE)$detected ; head(detections)
#mr <- whales
#tomap=TRUE ; toplot=TRUE
#max.km = 12
#bin.int=2

densities <- function(MR=MR,whales,detections,
                      max.km=12,step=.25,window=1,
                      scale.by.max=TRUE,tomap=FALSE,toplot=FALSE){
  if(tomap){
    plotKFS(area="Other",X=c(-129.6,-129.3),Y=c(53.1,53.35))
    points(MR$X,MR$Y,pch=16,cex=(.2),col="grey70")
    points(whales$X,mr$Y,pch=16,cex=(.5),col="steelblue3")
    points(detections$X,detected$Y,pch=16,cex=(.6),col="firebrick")
  }
  
  ###################################################
  # Calculate densities
  
  starts <- seq(0,(max.km - step),by=step) ; starts
  ends <- starts + window ; ends
  df <- data.frame(starts,ends) ; df
  df$mid <- (df$starts + df$ends) / 2 ; df
  pie <- data.frame() ; i=1
  for(i in 1:nrow(df)){
    dfi <- df[i,] ; dfi
    MRi <- nrow(MR[MR$km >= dfi$starts & MR$km < dfi$ends,]) ; MRi
    mri <- nrow(whales[whales$km >= dfi$starts & whales$km < dfi$ends,]) ; mri
    deti <- nrow(detections[detections$km >= dfi$starts & detections$km < dfi$ends,]) ; deti
    
    dfi <- data.frame(dfi,area=MRi,n.real=mri,n.seen=deti) ; dfi
    pie <- rbind(pie,dfi)
  }
  pie
  pie$d.real <- pie$n.real / pie$area
  pie$d.seen <- pie$n.seen / pie$area
  scaleby <- max(pie$d.seen,na.rm=TRUE) ; scaleby
  if(scaleby!=0){
    pie$d.seen <- pie$d.seen / scaleby
  }
  
  #if(toplot){
  #  plot(d.)
  #}
  
  return(pie)
}

#densities(MR,whales,detections,max.km=12,binint.=2,tomap=FALSE,toplot=FALSE)


###################################################
###################################################
# Simulate a set of scans

#whales <- whalesim(n=5000, d1=c(53.18,3,-129.37,20),showproc=FALSE) ; head(whales)
#n.pairs <- 3
#pop <-c(1,80)

create.scan.set <- function(n.pairs,whales,pop,MR,theta,max.km,step,window){
  scan1 <- scan2 <- data.frame()
  i=1
  for(i in 1:n.pairs){
    subwhales <- sample(1:nrow(whales),runif(1,pop[1],pop[2])) ; subwhales
    subwhales <- whales[subwhales,]
    j=1
    for(j in 1:2){
      detections <- simdetect(actual=subwhales,theta=theta,showproc=FALSE)$detected ; head(detections)
      D <- densities(MR,subwhales,detections,max.km=max.km,step=step,window=window,tomap=FALSE,toplot=FALSE) ; D
      cnames <- paste0("km",D$starts)
      scani <- data.frame(t(data.frame(D$d.seen)))
      names(scani) <- cnames ; scani
      scani$tot <- sum(D$n.seen) / sum(D$area)
      if(j==1){scan1 <- rbind(scan1,scani)}else{scan2 <- rbind(scan2,scani)} 
    }
  }
  scan1
  scan2
  return(list(scan1,scan2))
}

###################################################
###################################################
# Display Scan-Scan correlation by distance bin

scan.correlation <- function(pairs){
  hist.rate <- c(TRUE,FALSE,FALSE,FALSE)
  scan1 <- pairs[[1]] ; scan1
  scan2 <- pairs[[2]] ; scan2
  par(mfrow=c(4,4))
  par(mar=c(4.2,4.2,1,1))
  Rs <- vector()
  i=15
  for(i in 1:ncol(scan1)){
    ratio <- (scan1[,i] + 1) / (scan2[,i] + 1) ; ratio
    #if(hist.rate[i]){hist(ratio)}
    r2 <- sd(ratio) ; r2
    Rs <- c(Rs,r2)
  }
  par(mfrow=c(1,1))
  return(Rs)
}

###################################################
###################################################


