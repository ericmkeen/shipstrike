################################################################################
# p(encounter) - LNG Canada - fin whales
################################################################################

library(dplyr)
library(devtools)
#devtools::install_github('ericmkeen/bangarang')
library(bangarang)
#devtools::install_github('ericmkeen/shipstrike')
#library(shipstrike)
document()

source('tests/fw/parameters.R')
vessi <- readRDS('tests/vessels_cedar_lng_8_14knots.RData')

B <- 100 # number of times to run encounter
b <- 100 # number of iterations in each run

################################################################################

# Stage results
results <- data.frame()

# Loop through each vessel type
(types <- vessi$type %>% unique %>% sort)
(typi <- types[1])
for(typi in types){
    vm <- vessi %>% dplyr::filter(type == typi)
    nrow(vm)

    monthi <- 'all'

    if(nrow(vm)>0){

      # Loop through each diel period
      diels <- c('day','night')
      dieli <- 'day'
      for(dieli in diels){
        vmd <- vm %>% dplyr::filter(diel == dieli)
        nrow(vmd)
        if(nrow(vmd)>0){
          message('\nType ',typi,' :: Month ',monthi,' :: diel period ',dieli,' :: simulating encounters ....')

          # Setup ship parameters ================================================
          params.ship <- apply(vmd, 1, function(vmdi){
            #(vmdi <- vmd[1,])
            vmdi <- vmdi %>% as.character
            (speeds <- stringr::str_split(vmdi[4],'_') %>% unlist %>% as.numeric)
            (lengths <- stringr::str_split(vmdi[5],'_') %>% unlist %>% as.numeric)
            (widths <- stringr::str_split(vmdi[6],'_') %>% unlist %>% as.numeric)
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
                                              toplot=FALSE)

            # Process and tally encounters
            encounters$summary$encounters %>% sum
            (encs <- encounter_tally(encounters)$total)
            (p_encounter <- encs / b)

            # Add to building dataset
            (resulti <- data.frame(type = typi, month = monthi, diel = dieli, i = i, p_encounter))
            results <- rbind(results, resulti)

            # Save copy of data
            (fni <- paste0('tests/fw/impacts_cedar_lng/8_14_knots/p_encounter/Pencounter_',typi,'.RData'))
            saveRDS(results, file = fni)

          } # end of encounters B loop
          message('\nEncounter rates: ',paste(results$p_encounter[results$type == typi &
                                                                    results$month == monthi &
                                                                    results$diel == dieli], collapse=', '))
          par(mfrow=c(1,1))

        } # end of if nrow vmd > 0
      } # end of diels loop
    } # end of if nrow vm > 0
} # end of type loop

################################################################################

results$p_encounter %>% table
