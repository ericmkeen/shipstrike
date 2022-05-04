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
traffic <- readRDS('tests/vessels_2019.RData')
traffic %>% head
(vessels <- unique(traffic$type))
(channels <- unique(traffic$channel))

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
lfdir <- 'tests/fw/impacts_2019/p_encounter/'
(pencs <- list.files(lfdir))
(penc_files <- paste0(lfdir, pencs))

# P surface (depth distribution)
surf <- readRDS('tests/fw/p_surface.RData')
surf %>% head

# Avoidance
avoid <- readRDS('tests/p_avoidance.RData')
avoid

# Lethality
lethal <- readRDS('tests/p_lethal.RData')
lethal

################################################################################

months <- 1:12
diels <- c('day','night')
chi <- vi <- mi <- 6
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
                          month == monthi,
                          diel == dieli) %>%
            dplyr::select(-channel)) %>% head

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
          mr %>% dplyr::filter(cooccur == 1)
        }else{
          message('------   No p_encounter data for this channel-type-month-diel scenario!')
        }

        # Strike zone events  ==================================================
        message('------ strike zone potentialities ...')

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

        #  Collisions  1 =======================================================
        message('------ collisions (scenario 1: constant P(Avoidance) of 0.55) ...')

        mr$p_collision1 <- 1 - 0.55
        mr$collider1 <- runif(0,1,n=nrow(mr))
        mr$collide1 <- 0
        mr$collide1[mr$surf == 1 & mr$p_collision1 > mr$collider1] <- 1
        mr$collide1 %>% table

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
        mr$collide2 <- 0
        mr$collide2[mr$surf == 1 & mr$p_collision2 > mr$collider2] <- 1
        mr$collide2 %>% table

        #  Collisions  3 =======================================================
        message('------ collisions (scenario 3: no avoidance) ...')

        mr$collide3 <- mr$surf

        #  Collisions  4 =======================================================
        message('------ collisions (scenario 4: constant P(Avoidance) of 0.85) ...')

        mr$p_collision4 <- 1 - 0.85
        mr$collider4 <- runif(0,1,n=nrow(mr))
        mr$collide4 <- 0
        mr$collide4[mr$surf == 1 & mr$p_collision4 > mr$collider4] <- 1
        mr$collide4 %>% table

        # Mortalities  =========================================================
        message('------ mortalities ...')

        #(speeds <- unique(mr$speed))
        #morts <-  0.85 / (1 + exp(-1*(p_lethal$c1[1]+ (p_lethal$c2[1]*speeds))))
        #plot(morts~speeds, xlim=c(0,30), ylim=c(0,1))

        mr$p_lethality <- 0.85 / (1 + exp(-1*(p_lethal$c1[1]+ (p_lethal$c2[1]*mr$speed))))
        mr$mortr <- runif(0,1,n=nrow(mr))
        mr$mort1 <- 0
        mr$mort1[mr$collide1 == 1 & mr$p_lethality > mr$mortr] <- 1
        mr$mort2 <- 0
        mr$mort2[mr$collide2 == 1 & mr$p_lethality > mr$mortr] <- 1
        mr$mort3 <- 0
        mr$mort3[mr$collide3 == 1 & mr$p_lethality > mr$mortr] <- 1
        mr$mort4 <- 0
        mr$mort4[mr$collide4 == 1 & mr$p_lethality > mr$mortr] <- 1

        # Summarize iterations   ===============================================
        message('------ summarizing across spatial grid for each iteration ...')

        mrs <-
          mr %>%
          dplyr::group_by(iteration) %>%
          dplyr::summarize(cooccurrence = sum(cooccur),
                           encounter = sum(enc),
                           surface = sum(surf),
                           collision1 = sum(collide1),
                           collision2 = sum(collide2),
                           collision3 = sum(collide3),
                           collision4 = sum(collide4),
                           mortality1 = sum(mort1),
                           mortality2 = sum(mort2),
                           mortality3 = sum(mort3),
                           mortality4 = sum(mort4))

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

        # Report means
        message('------   mean of ', round(mean(mrs$cooccurrence),5),' co-occurrences')
        message('------   mean of ', round(mean(mrs$encounter),5),' close encounters')
        message('------   mean of ', round(mean(mrs$surface),5),' strike zone events')
        message('------   mean of ', round(mean(mrs$collision2),5),' collisions (after avoidance, using scenario 2)')
        message('------   mean of ', round(mean(mrs$mortality2),5),' mortalities')

        # Save to growing dataste
        MRS <- rbind(MRS, mrs)

        # Save to file   =============================================
        message('------ saving results ...')
        (fn <- paste0('tests/fw/impacts_2019/outcomes/',channi,'_',vessi,'.RData'))
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
