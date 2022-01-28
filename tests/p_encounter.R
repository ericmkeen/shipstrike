################################################################################
# p(encounter)
################################################################################

source('tests/parameters.R')
load('tests/vessels_sim.RData')
head(vessels_sim)

vessi <- vessels_sim
B <- 100 # number of times to run encounter
b <- 100 # number of iterations in each run

################################################################################

# Stage results
results <- data.frame()

# Loop through each month
months <- 1:12
monthi <- 1
for(monthi in months){
  vm <- vessi %>% dplyr::filter(month == monthi)
  nrow(vm)
  if(nrow(vm)>0){

    # Loop through each diel period
    diels <- c('day','night')
    dieli <- 'day'
    for(dieli in diels){
      vmd <- vm %>% dplyr::filter(diel == dieli)
      nrow(vmd)
      if(nrow(vmd)>0){
        message('Month ',monthi,' :: diel period ',dieli,' :: simulating encounters ....')

        # Setup ship parameters ================================================
        params.ship <- apply(vmd, 1, function(vmdi){
          vmdi <- vmdi %>% as.character
          (speeds <- stringr::str_split(vmdi[8],'_') %>% unlist %>% as.numeric)
          (lengths <- stringr::str_split(vmdi[9],'_') %>% unlist %>% as.numeric)
          (widths <- stringr::str_split(vmdi[10],'_') %>% unlist %>% as.numeric)
          dfi <- data.frame(v.ship = speeds, l.ship = lengths, w.ship = widths)
          return(dfi)
        })
        params.ship <- bind_rows(params.ship)
        head(params.ship)
        nrow(params.ship)

        # Draw random values for whale parameters
        (l.whale <- truncnorm::rtruncnorm(10000,a = length_min, b = length_max, mean = length_mean, sd = length_sd))
        (w.whale <- width_factor) #l.whale * width_factor)
        if(dieli == 'day'){
          (v.whale <- truncnorm::rtruncnorm(10000,a = speed_day_min, b = speed_day_max, mean = speed_day_mean, sd = speed_day_sd))
          (delta.sd <- rnorm(10000,mean = delta_day_mean, sd = delta_day_sd))
        }else{
          (v.whale <- truncnorm::rtruncnorm(10000,a = speed_night_min, b = speed_night_max, mean = speed_night_mean, sd = speed_night_sd))
          (delta.sd <- rnorm(10000,mean = delta_night_mean, sd = delta_night_sd))
        }
        (delta.sd <- exp(delta.sd)) # reverse log-transform

        # Begin encounter simulation loops =====================================
        pb <- txtProgressBar(1, B, style=3) # setup progress bar
        par(mfrow=c(2,2))
        p_encounters <- c()
        for(i in 1:B){
          setTxtProgressBar(pb, i) # update progress bar

          encounters <- encounter_simulator(params.ship=params.ship,
                                            v.whale=v.whale,
                                            l.whale=l.whale,
                                            w.whale=w.whale,
                                            delta.sd=delta.sd,
                                            B=b,
                                            save_records=FALSE,
                                            speedy=TRUE,
                                            verbose=FALSE,
                                            toplot=TRUE)

          # Process and tally encounters
          encounters$summary$encounters %>% sum
          (encs <- encounter_tally(encounters)$total)
          (p_encounter <- encs / b)

          # Add to building dataset
          (resulti <- data.frame(month = monthi, diel = dieli, p_encounter))
          results <- rbind(results, resulti)

          # Save copy of data
          save(results, file='tests/p_encounter.RData')

          # Update plot
          #par(mar=c(4.3,4.2,3,.5))
          #results$p_encounter %>% hist(main=paste0('After month ',monthi,': ',dieli,': iteration ',i), xlab = 'Encounter rate', breaks=20)
        } # end of encounters B loop
        message('Encounter rates: ',paste(results$p_encounter[results$month == monthi & results$diel == dieli], collapse=', '))
        par(mfrow=c(1,1))
      } # end of if nrow vmd > 0
    } # end of diels loop
  } # end of if nrow vm > 0
} # end of month loop

# Review
par(mar=c(4.2, 4.2, 3, .5))
p_encounters %>% hist


################################################################################
