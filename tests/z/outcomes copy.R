################################################################################
# outcomes - fin whales
################################################################################
# Analysis parameters

iterations <- 1000

################################################################################
# Libraries & package datasets

library(dplyr)
library(ggplot2)
library(bangarang)
library(devtools)
library(truncnorm)
load_all()

data(grid_kfs)
head(grid_kfs)

################################################################################
# Datasets / folder paths

# Traffic
traffic <- readRDS('tests/vessels_lng_canada_8_14knots.RData')
traffic %>% head
(vessels <- unique(traffic$type))
(channels <- unique(traffic$channel))

traffic %>% group_by(channel, type) %>% summarize(kms = sum(km), speed = mean(speed))

# Proportion of km2 covered
kms <- readRDS('tests/p_covered.RData')
kms %>% hist
kms %>% mean
kms %>% sd

# Whale density
load('tests/fw/dsm-estimate.RData')
whales <- grids
whales %>% head
#plotKFS()
#points(x=whales$x, y=whales$y, cex=whales$d_mean)

# Whale density bootstraps
load('tests/fw/dsm-bootstraps.RData')
boots <- bootstraps
boots %>% head
boots$month %>% table

# Seasonal posterior
load('tests/fw/seasonal_posterior.RData')
seasonal <- seasonal_boot
seasonal %>% names

# Encounter rate
lfdir <- 'tests/fw/impacts_lng_canada/8_14_knots/p_encounter/'
(pencs <- list.files(lfdir))
(penc_files <- paste0(lfdir, pencs))

# P surface (depth distribution)
surf <- readRDS('tests/fw/p_surface.RData')
surf %>% head

# Avoidance
avoid <- readRDS('tests/p_avoidance.RData')
avoid
(avoid <- avoid[c(2,4),])
(avoid$type <- c(paste(vessels[3:4], collapse=' | '),
                paste(vessels[1:2], collapse=' | ')))
avoid

# Lethality
lethal <- readRDS('tests/p_lethal.RData')
lethal
(lethal <- lethal[c(2,4),])
(lethal$type <- c(paste(vessels[3:4], collapse=' | '),
                 paste(vessels[1:2], collapse=' | ')))
lethal

################################################################################

months <- 1:12
diels <- c('day','night')
chi <- 2
vi <- 2
mi <- 8
di <- 1

for(chi in 1:length(channels)){
  (channi <- channels[chi])
  for(vi in 1:length(vessels)){
    (vessi <- vessels[vi])

    # Start new data file here
    MRS <- data.frame()

    for(mi in 1:length(months)){
      (monthi <- months[mi])
      for(di in 1:length(diels)){
        (dieli <- diels[di])
        #=======================================================================
        (traffi <-
           traffic %>%
           dplyr::filter(channel == channi,
                         type == vessi,
                         month == monthi,
                         diel == dieli)) %>% head

        # Begin iterations  ====================================================
        message('\n========== Channel ',channi,' :: Vessel type ',vessi,' :: Month ',monthi,' :: Diel period ',dieli,' ==========\n')
        mr <- data.frame()

        # Get scenario-specific probabilities   ================================

        (p_seasonal <- seasonal %>% dplyr::filter(month == monthi)) %>% head
        p_seasonal <- p_seasonal$scaled
        p_seasonal %>% mean
        p_seasonal %>% median

        (p_encounter <-
            readRDS(penc_files[grep(vessi, penc_files)]) %>%
            dplyr::filter(type == vessi,
                          diel == dieli) ) %>% head
        #par(mar=c(4.2,4.2,3,.5))
        #p_encounter$p_encounter %>% hist
        #p_encounter$p_encounter %>% mean

        (p_surface <- surf %>% dplyr::filter(diel == dieli)) %>% head

        (p_avoid <- avoid[grep(vessi, avoid$type) , ])

        (p_lethal <- lethal[grep(vessi, lethal$type) , ])

        # Co-occurrence ======================================================
        message('------ co-occurrences ...')

        if(nrow(traffi)>0){
          traffi %>% head
          traffi %>% nrow

          # Get grid cells transited
          (grid_cells <- traffi$grid_id)

          i=1
          pb <- txtProgressBar(1, max(c(2,length(grid_cells))), style=3) # setup progress bar
          # Loop through each grid cell
          for(i in 1:length(grid_cells)){
            (gi <- grid_cells[i])

            # Get B iterations of bootstrapped whale density for this grid cell
            (d_whale <- boots %>% dplyr::filter(grid_id == gi)) %>% head
            (dwi <- sample(d_whale$D, size=iterations, replace=TRUE))
            # treat negatives as 0

            # Add to results dataframe
            mri <- data.frame(grid_id = gi, iteration = 1:iterations, d_whale = dwi)
            mr <- rbind(mr, mri)
            setTxtProgressBar(pb, i)
          }
          mr %>% nrow
          mr %>% head

        }else{
          message('------   No vessel traffic for this channel-type-month-diel scenario!')
          mr <- data.frame(grid_id = NA, iteration = 1:iterations, d_whale = -1)
        }

        # Scale by seasonal abundance
        mr$seasonal <- sample(p_seasonal, size=nrow(mr), replace=TRUE)

        # Calculate p_whale
        mr$p_whale <- mr$d_whale * mr$seasonal

        # Test if whale is present
        mr$pwr <- runif(0,1,n=nrow(mr))
        mr$cooccur <- 0
        mr$cooccur[mr$p_whale > mr$pwr] <- 1
        mr$cooccur %>% table
        mr %>% head

        # Rescind some of these coccurrences based on the proportion of the
        # km2 grid that was actually covered.
        mr$p_covered <- sample(kms, size=nrow(mr), replace=TRUE) # randomly sample prop of km covered
        mr$pcovr <- runif(0, 1, n=nrow(mr)) # compare to a random draw
        (bads <- which(mr$cooccur == 1 & mr$p_covered < mr$pcovr)) %>% length # which need to be rescinded
        mr$cooccur[bads] <- 0 # rescind
        mr$cooccur %>% table # check effect


        # Close encounter  =====================================================
        message('\n------ close encounters ...')

        mr$penc <- -1
        mr$pencr <- runif(0,1,n=nrow(mr))
        mr$enc <- 0
        if(nrow(p_encounter)>0){
          p_encounter %>% head
          mr$penc <- sample(p_encounter$p_encounter, size=nrow(mr), replace=TRUE)
          mr$enc[mr$cooccur == 1 & mr$penc > mr$pencr] <- 1
          mr$enc %>% table
          mr$enc[mr$cooccur == 1] %>% table
          mr %>% dplyr::filter(cooccur == 1)
        }else{
          message('------   No p_encounter data for this channel-type-month-diel scenario!')
        }

        # Strike zone events 1 =================================================
        message('------ strike zone potentiality 1: draft == strike zone ...')

        if(nrow(traffi)>0){
          (drafts <- traffi$draft)
        }else{
          drafts <- c(0,0,0)
        }
        drafts

        mr$draft <- sample(drafts, size=nrow(mr), replace=TRUE)
        mr$pdraft <- rep(-1, times=nrow(mr))
        mr$pdraftr <- runif(0,1,n=nrow(mr))
        mr$surf <- 0
        (testi <- which(mr$enc == 1))
        if(length(testi)>0){
          i=1
          for(i in 1:length(testi)){
            (testii <- testi[i])
            (drafti <- round(mr$draft[testii]))
            (p_surface_mean <- p_surface$p_mean[p_surface$z == drafti])
            (p_surface_sd <- p_surface$p_sd[p_surface$z == drafti])
            (p_surfi <- truncnorm::rtruncnorm(n=1,
                                              a = 0,
                                              b=1,
                                              mean=p_surface_mean,
                                              sd=p_surface_sd))
            mr$pdraft[testii] <- p_surfi
          }
          mr$pdraft %>% table
          mr$surf[mr$enc == 1 & mr$pdraft > mr$pdraftr] <- 1
        }
        mr$surf %>% table
        mr %>% head

        # Strike zone events 2 ==================================================
        message('------ strike zone potentiality 2: draft x 1.5 == strike zone ...')

        if(nrow(traffi)>0){
          (drafts <- traffi$draft) * 1.5
        }else{
          drafts <- c(0,0,0)
        }
        drafts

        mr$draft2 <- sample(drafts, size=nrow(mr), replace=TRUE)
        mr$pdraft2 <- rep(-1, times=nrow(mr))
        mr$pdraftr2 <- runif(0,1,n=nrow(mr))
        mr$surf2 <- 0
        (testi <- which(mr$enc == 1))
        if(length(testi)>0){
          i=1
          for(i in 1:length(testi)){
            (testii <- testi[i])
            (drafti <- round(mr$draft2[testii]))
            (p_surface_mean <- p_surface$p_mean[p_surface$z == drafti])
            (p_surface_sd <- p_surface$p_sd[p_surface$z == drafti])
            (p_surfi <- truncnorm::rtruncnorm(n=1,
                                              a = 0,
                                              b=1,
                                              mean=p_surface_mean,
                                              sd=p_surface_sd))
            mr$pdraft2[testii] <- p_surfi
          }
          mr$pdraft2 %>% table
          mr$surf2[mr$enc == 1 & mr$pdraft2 > mr$pdraftr2] <- 1
        }
        mr$surf2 %>% table
        mr %>% head

        #  Collisions  1  =======================================================
        message('------ collisions (scenario 1: constant P(Avoidance) of 0.55) ...')

        mr$p_collision1 <- 1 - 0.55
        mr$collider1 <- runif(0,1,n=nrow(mr))

        mr$collide1.1 <- 0
        mr$collide1.1[mr$surf == 1 & mr$p_collision1 > mr$collider1] <- 1
        mr$collide1.1 %>% table

        mr$collide2.1 <- 0
        mr$collide2.1[mr$surf2 == 1 & mr$p_collision1 > mr$collider1] <- 1
        mr$collide2.1 %>% table

        #  Collisions  2 =======================================================
        message('------ collisions (scenario 2: avoidance ~ speed) ...')

        head(mr)
        if(nrow(traffi)>0){
          speeds <- traffi$speed
        }else{
          speeds <- c(0,0,0)
        }
        speeds

        mr$speed <- sample(speeds, size=nrow(mr), replace = TRUE)
        #speeds <- seq(0,15,length=1000)
        #collides <-  (1 / (1 + exp(-0.5*(speeds - 11.8))))
        #plot(collides~speeds)

        mr$p_collision2 <- p_avoid$asymptote / (1 + exp(p_avoid$c1[1] *(mr$speed - p_avoid$c2[1])))
        #hist(mr$p_collision2)
        mr$collider2 <- runif(0,1,n=nrow(mr))

        mr$collide1.2 <- 0
        mr$collide1.2[mr$surf == 1 & mr$p_collision2 > mr$collider2] <- 1
        mr$collide1.2 %>% table

        mr$collide2.2 <- 0
        mr$collide2.2[mr$surf2 == 1 & mr$p_collision2 > mr$collider2] <- 1
        mr$collide2.2 %>% table

        #  Collisions  3 =======================================================
        message('------ collisions (scenario 3: no avoidance) ...')

        mr$collide1.3 <- mr$surf
        mr$collide2.3 <- mr$surf2

        # Mortalities  =========================================================
        message('------ mortalities ...')

        #(speeds <- unique(mr$speed))
        #morts <-  1 / (1 + exp(-1*(p_lethal$c1[1]+ (p_lethal$c2[1]*speeds))))
        #plot(morts~speeds, xlim=c(0,30), ylim=c(0,1))
        p_lethal

        mr$p_lethality <- p_lethal$asymptote / (1 + exp(-1*(p_lethal$c1[1]+ (p_lethal$c2[1]*mr$speed))))
        #mr$p_lethality %>% unique
        #plot(mr$p_lethality~mr$speed, xlim=c(0,30), ylim=c(0,1))
        mr$mortr <- runif(0,1,n=nrow(mr))

        mr$mort1.1 <- 0
        mr$mort1.1[mr$collide1.1 == 1 & mr$p_lethality > mr$mortr] <- 1
        mr$mort1.2 <- 0
        mr$mort1.2[mr$collide1.2 == 1 & mr$p_lethality > mr$mortr] <- 1
        mr$mort1.3 <- 0
        mr$mort1.3[mr$collide1.3 == 1 & mr$p_lethality > mr$mortr] <- 1

        mr$mort2.1 <- 0
        mr$mort2.1[mr$collide2.1 == 1 & mr$p_lethality > mr$mortr] <- 1
        mr$mort2.2 <- 0
        mr$mort2.2[mr$collide2.2 == 1 & mr$p_lethality > mr$mortr] <- 1
        mr$mort2.3 <- 0
        mr$mort2.3[mr$collide2.3 == 1 & mr$p_lethality > mr$mortr] <- 1

        #mr$mort1.1 %>% table
        #mr$mort1.2 %>% table
        #mr$mort1.3 %>% table
        #mr$mort2.1 %>% table
        #mr$mort2.2 %>% table
        #mr$mort2.3 %>% table

        # Summarize iterations   ===============================================
        message('------ summarizing across spatial grid for each iteration ...')

        mrs <-
          mr %>%
          dplyr::group_by(iteration) %>%
          dplyr::summarize(cooccurrence = sum(cooccur),
                           encounter = sum(enc),
                           surface = sum(surf),
                           surface2 = sum(surf2),
                           collision1.1 = sum(collide1.1),
                           collision1.2 = sum(collide1.2),
                           collision1.3 = sum(collide1.3),
                           collision2.1 = sum(collide2.1),
                           collision2.2 = sum(collide2.2),
                           collision2.3 = sum(collide2.3),
                           mortality1.1 = sum(mort1.1),
                           mortality1.2 = sum(mort1.2),
                           mortality1.3 = sum(mort1.3),
                           mortality2.1 = sum(mort2.1),
                           mortality2.2 = sum(mort2.2),
                           mortality2.3 = sum(mort2.3))

        # Add scenario details
        mrs <- data.frame(species = 'fw',
                          year = 2019,
                          channel = channi,
                          vessel = vessi,
                          month = monthi,
                          diel = dieli,
                          mrs)

        mrs %>% head
        nrow(mrs)

        # Diagnostics
        if(FALSE){
          mr %>% head
          mr$grid_id %>% unique %>% length

          mr %>%
            select(d_whale:p_whale,
                   penc, draft, pdraft,
                   p_collision2,
                   p_lethality) %>%
              summarize(across(d_whale:p_lethality, mean)) %>% t %>% round(4)
        }


        # Report means
        message('------   mean of ', round(mean(mrs$cooccurrence),5),' co-occurrences')
        message('------   mean of ', round(mean(mrs$encounter),5),' close encounters')
        message('------   mean of ', round(mean(mrs$surface),5),' strike zone events')
        message('------   mean of ', round(mean(mrs$collision2.2),5),' collisions (after avoidance, using scenario 2.2)')
        message('------   mean of ', round(mean(mrs$mortality2.2),5),' mortalities')

        # Save to growing dataste
        MRS <- rbind(MRS, mrs)

        # Save to file   =============================================
        message('------ saving results ...')
        (fn <- paste0('tests/fw/impacts_lng_canada/8_14_knots/outcomes/',channi,'_',vessi,'.RData'))
        saveRDS(MRS, file=fn)

        #MRS %>% head
        #MRS %>% group_by(diel) %>% summarize(across(cooccurrence:mortality, mean))
        #ggplot(MRS, aes(x=factor(month), y=cooccurrence, color=diel)) + geom_violin()

        #=======================================================================
      } # end of diel periods
    }# end of months
  } # end of vessel types
} # end of channels

################################################################################
