#' Compile results from outcomes function
#'
#' @param outcome_dir dir
#'
#' @return A big list.
#' @export
#'
compile_results <- function(outcome_dir){

  if(FALSE){
    library(dplyr)
    library(ggplot2)
    library(devtools)
    load_all()
    outcome_dir <- 'tests/fw/impacts_2019/outcomes/'
  }

  suppressWarnings({

    MR <- list()

    ################################################################################
    # Combine results

    message('Combining outcomes ...')

    mr <- data.frame()
    for(j in 1:length(outcome_dir)){
      outcome_dirj <- outcome_dir[j]
      message('--- ',outcome_dirj)
      lf <- list.files(outcome_dirj)
      (lf <- paste0(outcome_dirj,lf))
      for(i in 1:length(lf)){
        (lfi <- lf[i])
        message('--- --- ',lfi)
        mri <- readRDS(lfi)
        mr <- rbind(mr, mri)
      }
    }

    nrow(mr)

    mr$channel[mr$channel == 'CAA'] <- 'Caamano'
    mr$channel[mr$channel == 'EST'] <- 'Estevan'
    mr$channel[mr$channel == 'CMP'] <- 'Campania'
    mr$channel[mr$channel == 'SQU'] <- 'Squally'
    mr$channel[mr$channel == 'WHA'] <- 'Whale'
    mr$channel[mr$channel == 'WRI'] <- 'Wright'
    mr$channel[mr$channel == 'MCK'] <- 'McKay'
    mr$channel[mr$channel == 'VER'] <- 'Verney'
    mr$channel <- factor(mr$channel, levels=c('Caamano','Estevan','Campania','Squally','Whale','Wright','McKay','Verney'))

    MR$df <- mr

    ################################################################################
    ################################################################################
    ################################################################################
    # Grand total table ============================================================

    message('Grand data table ...')

    melted <- data.table::melt(mr, id.vars=1:7)
    head(melted)

    grand_table <-
      melted %>%
      group_by(species, year, variable, iteration) %>%
      summarize(value = sum(value)) %>%
      group_by(species, year, variable) %>%
      summarize(mean = round(mean(value),2),
                median = median(value),
                q5 = quantile(value, .05),
                q95 = quantile(value,.95),
                q20 = quantile(value,.2))

    grand_table
    MR$grand_table <- grand_table

    # Grand probs ==================================================================

    message('Grand chances table ...')

    mrs <-
      mr %>%
      group_by(iteration) %>%
      summarize(across(cooccurrence:mortality2.3, sum)) %>%
      rename(collision = collision2.2,
             mortality = mortality2.2) %>%
      select(-iteration)

    m0 <- data.frame(Chances_of = 'Zero',
                     Collisions = round(100*(length(which(mrs$collision == 0))/ nrow(mrs)),1),
                     Mortalities = round(100*(length(which(mrs$mortality == 0))/ nrow(mrs)),1))
    m1 <- data.frame(Chances_of = 'At least 1',
                     Collisions = round(100*(length(which(mrs$collision >= 1))/ nrow(mrs)),1),
                     Mortalities = round(100*(length(which(mrs$mortality >= 1))/ nrow(mrs)),1))
    m2 <- data.frame(Chances_of = 'At least 2',
                     Collisions = round(100*(length(which(mrs$collision >= 2))/ nrow(mrs)),1),
                     Mortalities = round(100*(length(which(mrs$mortality >= 2))/ nrow(mrs)),1))
    m3 <- data.frame(Chances_of = 'At least 3',
                     Collisions = round(100*(length(which(mrs$collision >= 3))/ nrow(mrs)),1),
                     Mortalities = round(100*(length(which(mrs$mortality >= 3))/ nrow(mrs)),1))
    m4 <- data.frame(Chances_of = 'At least 4',
                     Collisions = round(100*(length(which(mrs$collision >= 4))/ nrow(mrs)),1),
                     Mortalities = round(100*(length(which(mrs$mortality >= 4))/ nrow(mrs)),1))
    m5 <- data.frame(Chances_of = 'At least 5',
                     Collisions = round(100*(length(which(mrs$collision >= 5))/ nrow(mrs)),1),
                     Mortalities = round(100*(length(which(mrs$mortality >= 5))/ nrow(mrs)),1))

    chances <- rbind(m0, m1, m2, m3, m4, m5)
    MR$chances <- chances

    # Grand total histograms =======================================================

    message('Grand total histograms ...')

    # All ========

    melted <-
      data.table::melt(mr, id.vars=1:7) %>%
      group_by(variable, iteration) %>%
      summarize(value = sum(value))

    head(melted)

    gg_outcomes_all <-
      ggplot2::ggplot(melted,
                      ggplot2::aes(x=value)) +
      ggplot2::geom_bar(stat='count', width=1, alpha=.7, fill='darkslategray') +
      ggplot2::ylab('Frequency') +
      ggplot2::xlab('Predicted outcomes') +
      ggplot2::facet_wrap(~variable, scales='free') +
      ggplot2::theme_light() +
      ggplot2::theme(strip.text.x = ggplot2::element_text(size=9, color='grey95', face='bold'),
                     strip.background = ggplot2::element_rect(fill="grey60"))

    gg_outcomes_all
    MR$gg_outcomes_all <- gg_outcomes_all

    # Polished ======

    head(melted)
    keeps <- c('cooccurrence', 'encounter', 'surface', 'collision2.2', 'mortality2.2')
    melti <- melted %>%
      dplyr::filter(as.character(variable) %in% keeps) %>%
      mutate(variable = as.character(variable))
    nrow(melti)

    melti$variable %>% unique
    melti$variable[as.character(melti$variable) == 'cooccurrence'] <- 'Cooccurrence'
    melti$variable[as.character(melti$variable) == 'encounter'] <- 'Close encounter'
    melti$variable[as.character(melti$variable) == 'surface'] <- 'Strike-zone event'
    melti$variable[as.character(melti$variable) == 'collision2.2'] <- 'Collision'
    melti$variable[as.character(melti$variable) == 'mortality2.2'] <- 'Mortality'
    melti$variable %>% as.character %>% unique
    melti$variable <- factor(melti$variable, levels = c('Cooccurrence', 'Close encounter', 'Strike-zone event', 'Collision', 'Mortality'))

    gg_outcomes <-
      ggplot2::ggplot(melti,
                      ggplot2::aes(x=value)) +
      ggplot2::geom_bar(stat='count', width=1, alpha=.7, fill='darkslategray') +
      ggplot2::ylab('Frequency') +
      ggplot2::xlab('Predicted outcomes') +
      ggplot2::facet_wrap(~variable, scales='free') +
      ggplot2::theme_light() +
      ggplot2::theme(strip.text.x = ggplot2::element_text(size=9, color='grey95', face='bold'),
                     strip.background = ggplot2::element_rect(fill="grey60"))

    gg_outcomes
    MR$gg_outcomes <- gg_outcomes

    ################################################################################
    ################################################################################
    # Summarization plots by scenario

    message('Summarizing raw results of monthly patterns ...')

    melted <- data.table::melt(mr, id.vars=1:7)
    head(melted)

    # Monthly patterns =============================================================

    melti <-
      melted %>%
      group_by(variable, month, iteration) %>%
      summarize(value = sum(value))

    ggraw_monthly <-
      ggplot(melti, aes(x=value, y=factor(month))) +
      geom_jitter(height = .35, width=.4, alpha=.05, cex=.3, color = 'darkblue') +
      xlab('Number of interactions') +
      ylab('Month') +
      facet_wrap(~variable, scales='free_x')

    ggraw_monthly
    MR$ggraw_monthly <- ggraw_monthly

    # Vessel patterns =============================================================

    message('Summarizing raw results of vessel patterns ...')

    melti <-
      melted %>%
      group_by(variable, vessel, iteration) %>%
      summarize(value = sum(value))

    ggraw_vessel <-
      ggplot(melti, aes(x=value, y=vessel)) +
      geom_jitter(height = .35, width=.4, alpha=.05, cex=.3, color = 'darkblue') +
      xlab('Number of interactions') +
      ylab(NULL) +
      facet_wrap(~variable, scales='free_x')

    ggraw_vessel
    MR$ggraw_vessel <- ggraw_vessel

    # Channel patterns =============================================================

    message('Summarizing raw results of channel patterns ...')

    melti <-
      melted %>%
      group_by(variable, channel, iteration) %>%
      summarize(value = sum(value))

    ggraw_channel <-
      ggplot(melti, aes(x=value, y=channel)) +
      geom_jitter(height = .35, width=.4, alpha=.05, cex=.3, color = 'darkblue') +
      xlab('Number of interactions') +
      ylab(NULL) +
      facet_wrap(~variable, scales='free_x')

    ggraw_channel
    MR$ggraw_channel <- ggraw_channel

    # Diel patterns =============================================================

    message('Summarizing raw results of diel patterns ...')

    melti <-
      melted %>%
      group_by(variable, diel, iteration) %>%
      summarize(value = sum(value))

    ggraw_diel <-
      ggplot(melti, aes(x=value, y=diel)) +
      geom_jitter(height = .35, width=.4, alpha=.1, cex=.8, color = 'darkblue') +
      xlab('Number of interactions') +
      ylab('Diel period') +
      facet_wrap(~variable, scales='free_x')

    ggraw_diel
    MR$ggraw_diel <- ggraw_diel


    ################################################################################
    ################################################################################
    # Melt function for share of risk calculations

    message('Melting data to determine shares of risk ...')

    melt_outcomes <- function(mr,
                              events = c('cooccurrence',
                                         'encounter',
                                         'surface', 'surface2',
                                         'collision1.1','collision1.2','collision1.3',
                                         'collision2.1','collision2.2','collision2.3',
                                         'mortality1.1','mortality1.2','mortality1.3',
                                         'mortality2.1','mortality2.2','mortality2.3')){
      dfe <- data.frame()
      for(i in 1:length(events)){
        (eventi <- events[i])
        message(eventi)
        (keeper <- which(names(mr)==eventi))
        (evi <- mr[,keeper] %>% as.data.frame)
        evi <- evi[,1] %>% as.numeric
        dfevi <- mr %>% select(species:iteration)
        dfevi$event = eventi
        dfevi$outcome = evi
        dfe <- rbind(dfe, dfevi)
      }

      message('re-factoring events ...')
      dfe$event <- factor(dfe$event, levels = events)
      return(dfe)
    }

    mrm <- melt_outcomes(mr)
    nrow(mrm)
    head(mrm)

    # Share of outcomes by vessel type   ===========================================

    message('Calculating share of risks by vessel type ...')

    mri <-
      mrm %>%
      group_by(event, month, channel, vessel, diel) %>%
      summarize(iteration = 1:1000,
                outcome = outcome[sample(1:n(),size=1000, replace=TRUE)])

    mri %>% nrow
    mri %>% data.frame %>% head(100)

    mrii <-
      mri %>%
      group_by(vessel, event, iteration) %>%
      summarize(outcome = sum(outcome)) %>%
      group_by(vessel, event) %>%
      summarize(mean = mean(outcome),
                median = median(outcome),
                q5 = quantile(outcome, .05),
                q95 = quantile(outcome, .95),
                q20 = quantile(outcome, .2))

    mriii <-
      mrii %>%
      group_by(event) %>%
      mutate(tot = sum(mean)) %>%
      ungroup() %>%
      mutate(prop = round(100*(mean / tot))) %>%
      mutate(prop = ifelse(!is.finite(prop), 0, prop))
    mriii

    (events <- unique(mrii$event))
    i=2
    for(i in 1:length(events)){
      (eventi <- events[i])
      mriv <-
        mriii %>%
        dplyr::filter(event == eventi) %>%
        select(vessel, prop)
      mriv
      if(i==1){
        mrv <- mriv
      }else{
        mrv <- left_join(mrv, mriv, by='vessel')
      }
      names(mrv) <- gsub('prop',as.character(eventi),names(mrv))
      #names(mrv)[which(names(mrv)=='prop')] <- as.character(eventi)
      mrv
    }

    (share_vessels <- mrv %>% as.data.frame)
    MR$share_vessels <- share_vessels

    melted <- data.table::melt(share_vessels, id.vars=1)

    ggshare_vessels <-
      ggplot(melted, aes(x=value, y=vessel)) +
      geom_col(fill='darkblue', alpha=.7) +
      xlab('Percent share of predicted outcomes') +
      ylab(NULL) +
      facet_wrap(~variable, scales='fixed') +
      labs(title = 'Share of risk by vessel type')

    ggshare_vessels
    MR$ggshare_vessels <- ggshare_vessels

    # Share of outcomes by channel =================================================

    message('Calculating share of risks by channel ...')

    mri <-
      mrm %>%
      group_by(event, month, channel, vessel, diel) %>%
      summarize(iteration = 1:1000,
                outcome = outcome[sample(1:n(),size=1000, replace=TRUE)])

    mri %>% nrow
    mri %>% data.frame %>% head(100)

    mrii <-
      mri %>%
      group_by(channel, event, iteration) %>%
      summarize(outcome = sum(outcome)) %>%
      group_by(channel, event) %>%
      summarize(mean = mean(outcome),
                median = median(outcome),
                q5 = quantile(outcome, .05),
                q95 = quantile(outcome, .95),
                q20 = quantile(outcome, .2))

    mriii <-
      mrii %>%
      group_by(event) %>%
      mutate(tot = sum(mean)) %>%
      ungroup() %>%
      mutate(prop = round(100*(mean / tot))) %>%
      mutate(prop = ifelse(!is.finite(prop), 0, prop))
    mriii

    (events <- unique(mrii$event))
    i=2
    for(i in 1:length(events)){
      (eventi <- events[i])
      mriv <-
        mriii %>%
        dplyr::filter(event == eventi) %>%
        select(channel, prop)
      mriv
      if(i==1){
        mrv <- mriv
      }else{
        mrv <- left_join(mrv, mriv, by='channel')
      }
      names(mrv) <- gsub('prop',as.character(eventi),names(mrv))
      #names(mrv)[which(names(mrv)=='prop')] <- as.character(eventi)
      mrv
    }

    (share_channels <- mrv %>% as.data.frame)
    MR$share_channels <- share_channels

    melted <- data.table::melt(share_channels, id.vars=1)

    ggshare_channels <-
      ggplot(melted, aes(x=value, y=channel)) +
      geom_col(fill='darkblue', alpha=.7) +
      xlab('Percent share of predicted outcomes') +
      ylab(NULL) +
      facet_wrap(~variable, scales='fixed') +
      labs(title = 'Share of risk by waterway')

    ggshare_channels
    MR$ggshare_channels <- ggshare_channels


    # Share month ==================================================================

    message('Calculating share of risks by month ...')

    mri <-
      mrm %>%
      group_by(event, month, channel, vessel, diel) %>%
      summarize(iteration = 1:1000,
                outcome = outcome[sample(1:n(),size=1000, replace=TRUE)])

    mri %>% nrow
    mri %>% data.frame %>% head(100)

    mrii <-
      mri %>%
      group_by(month, event, iteration) %>%
      summarize(outcome = sum(outcome)) %>%
      group_by(month, event) %>%
      summarize(mean = mean(outcome),
                median = median(outcome),
                q5 = quantile(outcome, .05),
                q95 = quantile(outcome, .95),
                q20 = quantile(outcome, .2))
    mrii

    mriii <-
      mrii %>%
      group_by(event) %>%
      mutate(tot = sum(mean)) %>%
      ungroup() %>%
      mutate(prop = round(100*(mean / tot))) %>%
      mutate(prop = ifelse(!is.finite(prop), 0, prop))
    mriii

    (events <- unique(mrii$event))
    i=1
    for(i in 1:length(events)){
      (eventi <- events[i])
      mriv <-
        mriii %>%
        dplyr::filter(event == eventi) %>%
        select(month, prop)
      mriv
      if(i==1){
        mrv <- mriv
      }else{
        mrv <- left_join(mrv, mriv, by='month')
      }
      mrv
      names(mrv) <- gsub('prop',as.character(eventi),names(mrv))
      mrv
    }

    (share_months <- mrv %>% as.data.frame)
    MR$share_months <- share_months

    # Plot it
    melted <- data.table::melt(share_months, id.vars=1)

    ggshare_months <-
      ggplot(melted, aes(y=factor(month), x=value)) +
      geom_col(fill='darkblue', alpha=.7) +
      scale_y_discrete(limits=factor(12:1)) +
      xlab('Percent share of predicted outcomes') +
      ylab('Month') +
      facet_wrap(~variable, scales='fixed') +
      labs(title = 'Share of risk by calendar month')

    ggshare_months
    MR$ggshare_months <- ggshare_months

    # Share diel ===================================================================

    message('Calculating share of risks by diel period ...')

    mri <-
      mrm %>%
      group_by(event, month, channel, vessel, diel) %>%
      summarize(iteration = 1:1000,
                outcome = outcome[sample(1:n(),size=1000, replace=TRUE)])

    mri %>% nrow
    mri %>% data.frame %>% head(100)

    mrii <-
      mri %>%
      group_by(diel, event, iteration) %>%
      summarize(outcome = sum(outcome)) %>%
      group_by(diel, event) %>%
      summarize(mean = mean(outcome),
                median = median(outcome),
                q5 = quantile(outcome, .05),
                q95 = quantile(outcome, .95),
                q20 = quantile(outcome, .2))
    mrii

    mriii <-
      mrii %>%
      group_by(event) %>%
      mutate(tot = sum(mean)) %>%
      ungroup() %>%
      mutate(prop = round(100*(mean / tot))) %>%
      mutate(prop = ifelse(!is.finite(prop), 0, prop))
    mriii

    (events <- unique(mrii$event))
    i=1
    for(i in 1:length(events)){
      (eventi <- events[i])
      mriv <-
        mriii %>%
        dplyr::filter(event == eventi) %>%
        select(diel, prop)
      mriv
      if(i==1){
        mrv <- mriv
      }else{
        mrv <- left_join(mrv, mriv, by='diel')
      }
      mrv
      names(mrv) <- gsub('prop',as.character(eventi),names(mrv))
      mrv
    }

    (share_diel <- mrv %>% as.data.frame)
    MR$share_diel <- share_diel

    # Plot it
    melted <- data.table::melt(share_diel, id.vars=1)

    ggshare_diel <-
      ggplot(melted, aes(y=factor(diel), x=value)) +
      geom_col(fill='darkblue', alpha=.7) +
      xlab('Percent share of predicted outcomes') +
      ylab('Diel period') +
      facet_wrap(~variable, scales='fixed') +
      labs(title = 'Share of risk by night vs. day')

    ggshare_diel
    MR$ggshare_diel <- ggshare_diel

    ################################################################################
    ################################################################################
    # Cumulative projections

    # Function =================================================================

    proj_plot <- function(outcome, mr){
      #outcome <- c('collision2.1', 'collision2.2', 'collision2.3')
      melted <- data.table::melt(mr, id.vars=1:7)
      head(melted)
      melti <-
        melted %>%
        dplyr::filter(variable %in% outcome) %>%
        group_by(variable, month, iteration) %>%
        summarize(value = sum(value))
      melti %>% head
      nrow(melti)

      df <- data.frame()
      years <- 2030:2040
      months <- unique(melti$month)
      y <- m <- o <- 1
      for(y in 1:length(years)){
        (yeari <- years[y])
        message(yeari)
        for(m in 1:length(months)){
          (monthi <- months[m])
          message('--- ',monthi)
          (mi <- melti %>% dplyr::filter(month == monthi))
          for(o in 1:length(outcome)){
            (outcomi <- outcome[o])
            message('--- ---',outcomi)
            oi <- mi %>% dplyr::filter(variable == outcomi)
            (vali <- sample(oi$value,size=100, replace=TRUE))
            dfi <- data.frame(year = yeari,
                              month = monthi,
                              outcome = outcomi,
                              iteration = 1:100,
                              value = vali)
            df <- rbind(df, dfi)
          }
        }
      }

      head(df)
      dfcum <-
        df %>%
        mutate(yfrac = year + ((month-.5) / 12)) %>%
        group_by(outcome, iteration) %>%
        mutate(cum = cumsum(value)) %>%
        ungroup()

      dfcum %>% tail

      dfcumsum <-
        dfcum %>%
        group_by(outcome, yfrac) %>%
        summarize(mn = mean(cum),
                  q20 = quantile(cum, .2),
                  lci = quantile(cum, .05),
                  uci = quantile(cum, .95))

      ggproj <-
        ggplot(dfcumsum) +
        geom_ribbon(mapping = aes(x=yfrac, ymin = lci, ymax = uci, fill=outcome), alpha=.2) +
        geom_line(mapping = aes(x=yfrac, y=mn, color=outcome), alpha=.5, lwd=1.5) +
        geom_line(mapping = aes(x=yfrac, y=q20, color=outcome), alpha=.9, lty=3) +
        scale_x_continuous(breaks=2030:2041) +
        ylab(outcome[1]) +
        xlab(NULL) +
        geom_line(data=data.frame(yfrac = 2030:2041, mn = 0:11), mapping = aes(x=yfrac, y=mn), lty=3)

      # Summarize for 2033, 2034, 2039
      # mean, median, lci, uci, 80% confidence
      summproj <-
        dfcum %>%
        group_by(outcome) %>%
        summarize(yr3 = paste0(round(mean(cum[year == 2032 & month == 12]),1),
                                    ' (',
                                    round(quantile(cum[year == 2032 & month == 12], 0.05)),
                                    '-',
                                    round(quantile(cum[year == 2032 & month == 12], 0.95)),
                                    ')'),
                  yr3_q20 = quantile(cum[year == 2032 & month == 12], 0.2),
                  yr5 = paste0(round(mean(cum[year == 2034 & month == 12]),1),
                               ' (',
                               round(quantile(cum[year == 2034 & month == 12], 0.05)),
                               '-',
                               round(quantile(cum[year == 2034 & month == 12], 0.95)),
                               ')'),
                  yr5_q20 = quantile(cum[year == 2039 & month == 12], 0.2),
                  yr10 = paste0(round(mean(cum[year == 2039 & month == 12]),1),
                               ' (',
                               round(quantile(cum[year == 2039 & month == 12], 0.05)),
                               '-',
                               round(quantile(cum[year == 2039 & month == 12], 0.95)),
                               ')'),
                  yr10_q20 = quantile(cum[year == 2039 & month == 12], 0.2))
      summproj

      return(list(proj = summproj, ggproj = ggproj))
    }

    projections <- data.frame()

    proji <- proj_plot(outcome = 'cooccurrence', mr)
    (ggproj_cooccurrence <- proji$ggproj)
    (projections <- rbind(projections, proji$proj))

    proji <- proj_plot(outcome = 'encounter', mr)
    (ggproj_encounter <- proji$ggproj)
    (projections <- rbind(projections, proji$proj))

    proji <- proj_plot(outcome = c('surface','surface2'), mr)
    (ggproj_surface <- proji$ggproj)
    (projections <- rbind(projections, proji$proj))

    proji <- proj_plot(outcome = c('collision2.1','collision2.2','collision2.3'), mr)
    (ggproj_collision <- proji$ggproj)
    (projections <- rbind(projections, proji$proj))

    proji <- proj_plot(outcome = c('mortality2.1','mortality2.2','mortality2.3'), mr)
    (ggproj_mortality <- proji$ggproj)
    (projections <- rbind(projections, proji$proj))

    MR$projections <- projections
    MR$ggproj_cooccurrence <- ggproj_cooccurrence
    MR$ggproj_encounter <- ggproj_encounter
    MR$ggproj_surface <- ggproj_surface
    MR$ggproj_collision <- ggproj_collision
    MR$ggproj_mortality <- ggproj_mortality

    ################################################################################
    ################################################################################
    names(MR)
  })

  return(MR)
}
