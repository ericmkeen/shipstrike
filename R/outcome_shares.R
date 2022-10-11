#' Share of risk by vessel type, channel, month, and diel period
#'
#' This function uses a bootstrapping routine (described in Keen et al. 2023)
#' to estimate the share of risk attributable to each level for vessel type,
#' channel, month, and diel period. It does this for each outcome type
#' (cooccurrence, close encounter, …, mortality).
#'
#' @param outcomes A `data.frame` of results from the `shipstrike::gather_outcomes()` function.
#'
#' @return A named `list`.
#' @export
#' @import dplyr
#' @import data.table
#' @import ggplot2
#'
outcome_shares <- function(outcomes, verbose=TRUE){

  suppressWarnings({

    # Rename for simplicity
    mr <- outcomes

    if(verbose){message('Melting outcome data ...')}
    mrm <- outcome_melt(mr)

    # Stage results object
    MR <- list()

    mri <-
      mrm %>%
      group_by(event, month, channel, vessel, diel) %>%
      summarize(iteration = 1:1000,
                outcome = outcome[sample(1:n(),size=1000, replace=TRUE)])
    mri %>% nrow
    mri %>% data.frame %>% head(100)

    # Share of outcomes by vessel type   ===========================================

    if(verbose){message('Calculating share of risks by vessel type ...')}

    mrii <-
      mri %>%
      group_by(vessel, event, iteration) %>%
      summarize(outcome = sum(outcome)) %>%
      group_by(vessel, event) %>%
      summarize(mean = mean(outcome),
                median = median(outcome),
                q5 = quantile(outcome, .05),
                q95 = quantile(outcome, .95),
                q20 = quantile(outcome, .2)) %>%
      group_by(event) %>%
      mutate(tot = sum(mean)) %>%
      ungroup() %>%
      mutate(prop = round(100*(mean / tot))) %>%
      mutate(prop = ifelse(!is.finite(prop), 0, prop)) %>%
      group_by(event, vessel) %>%
      summarize(prop = prop)

    mrii

    # (events <- unique(mrii$event))
    # i=2
    # for(i in 1:length(events)){
    #   (eventi <- events[i])
    #   mriv <-
    #     mriii %>%
    #     dplyr::filter(event == eventi) %>%
    #     dplyr::select(vessel, prop)
    #   mriv
    #   if(i==1){
    #     mrv <- mriv
    #   }else{
    #     mrv <- left_join(mrv, mriv, by='vessel')
    #   }
    #   names(mrv) <- gsub('prop',as.character(eventi),names(mrv))
    #   #names(mrv)[which(names(mrv)=='prop')] <- as.character(eventi)
    #   mrv
    # }
    # (share_vessels <- mrv %>% as.data.frame)
    #
    # melted <- data.table::melt(share_vessels, id.vars=1)
    #
    # ggshare_vessels <-
    #   ggplot(melted, aes(x=value, y=vessel)) +
    #   geom_col(fill='darkblue', alpha=.7) +
    #   xlab('Percent share of predicted outcomes') +
    #   ylab(NULL) +
    #   facet_wrap(~variable, scales='fixed') +
    #   labs(title = 'Share of risk by vessel type')

    MR$vessel <- mrii
    #MR$vessel <- list(table = share_vessels, plot = ggshare_vessels)

    # Share of outcomes by channel =================================================

    if(verbose){message('Calculating share of risks by channel ...')}

    mrii <-
      mri %>%
      group_by(channel, event, iteration) %>%
      summarize(outcome = sum(outcome)) %>%
      group_by(channel, event) %>%
      summarize(mean = mean(outcome),
                median = median(outcome),
                q5 = quantile(outcome, .05),
                q95 = quantile(outcome, .95),
                q20 = quantile(outcome, .2)) %>%
      group_by(event) %>%
      mutate(tot = sum(mean)) %>%
      ungroup() %>%
      mutate(prop = round(100*(mean / tot))) %>%
      mutate(prop = ifelse(!is.finite(prop), 0, prop)) %>%
      group_by(event, channel) %>%
      summarize(prop = prop)

    mrii
    MR$channel <- mrii

    # mri <-
    #   mrm %>%
    #   group_by(event, month, channel, vessel, diel) %>%
    #   summarize(iteration = 1:1000,
    #             outcome = outcome[sample(1:n(),size=1000, replace=TRUE)])
    #
    # mri %>% nrow
    # mri %>% data.frame %>% head(100)
    #
    # mrii <-
    #   mri %>%
    #   group_by(channel, event, iteration) %>%
    #   summarize(outcome = sum(outcome)) %>%
    #   group_by(channel, event) %>%
    #   summarize(mean = mean(outcome),
    #             median = median(outcome),
    #             q5 = quantile(outcome, .05),
    #             q95 = quantile(outcome, .95),
    #             q20 = quantile(outcome, .2))
    #
    # mriii <-
    #   mrii %>%
    #   group_by(event) %>%
    #   mutate(tot = sum(mean)) %>%
    #   ungroup() %>%
    #   mutate(prop = round(100*(mean / tot))) %>%
    #   mutate(prop = ifelse(!is.finite(prop), 0, prop))
    # mriii
    #
    # (events <- unique(mrii$event))
    # i=2
    # for(i in 1:length(events)){
    #   (eventi <- events[i])
    #   mriv <-
    #     mriii %>%
    #     dplyr::filter(event == eventi) %>%
    #     dplyr::select(channel, prop)
    #   mriv
    #   if(i==1){
    #     mrv <- mriv
    #   }else{
    #     mrv <- left_join(mrv, mriv, by='channel')
    #   }
    #   names(mrv) <- gsub('prop',as.character(eventi),names(mrv))
    #   #names(mrv)[which(names(mrv)=='prop')] <- as.character(eventi)
    #   mrv
    # }
    # (share_channels <- mrv %>% as.data.frame)
    #
    # melted <- data.table::melt(share_channels, id.vars=1)
    #
    # ggshare_channels <-
    #   ggplot(melted, aes(x=value, y=channel)) +
    #   geom_col(fill='darkblue', alpha=.7) +
    #   xlab('Percent share of predicted outcomes') +
    #   ylab(NULL) +
    #   facet_wrap(~variable, scales='fixed') +
    #   labs(title = 'Share of risk by waterway')
    #
    # MR$channel <- list(table = share_channels, plot = ggshare_channels)



    # Share month ==================================================================

    if(verbose){message('Calculating share of risks by month ...')}

    mrii <-
      mri %>%
      group_by(month, event, iteration) %>%
      summarize(outcome = sum(outcome)) %>%
      group_by(month, event) %>%
      summarize(mean = mean(outcome),
                median = median(outcome),
                q5 = quantile(outcome, .05),
                q95 = quantile(outcome, .95),
                q20 = quantile(outcome, .2)) %>%
      group_by(event) %>%
      mutate(tot = sum(mean)) %>%
      ungroup() %>%
      mutate(prop = round(100*(mean / tot))) %>%
      mutate(prop = ifelse(!is.finite(prop), 0, prop)) %>%
      group_by(event, month) %>%
      summarize(prop = prop)

    MR$month <- mrii

    # mri <-
    #   mrm %>%
    #   group_by(event, month, channel, vessel, diel) %>%
    #   summarize(iteration = 1:1000,
    #             outcome = outcome[sample(1:n(),size=1000, replace=TRUE)])
    #
    # mri %>% nrow
    # mri %>% data.frame %>% head(100)
    #
    # mrii <-
    #   mri %>%
    #   group_by(month, event, iteration) %>%
    #   summarize(outcome = sum(outcome)) %>%
    #   group_by(month, event) %>%
    #   summarize(mean = mean(outcome),
    #             median = median(outcome),
    #             q5 = quantile(outcome, .05),
    #             q95 = quantile(outcome, .95),
    #             q20 = quantile(outcome, .2))
    # mrii
    #
    # mriii <-
    #   mrii %>%
    #   group_by(event) %>%
    #   mutate(tot = sum(mean)) %>%
    #   ungroup() %>%
    #   mutate(prop = round(100*(mean / tot))) %>%
    #   mutate(prop = ifelse(!is.finite(prop), 0, prop))
    # mriii
    #
    # (events <- unique(mrii$event))
    # i=1
    # for(i in 1:length(events)){
    #   (eventi <- events[i])
    #   mriv <-
    #     mriii %>%
    #     dplyr::filter(event == eventi) %>%
    #     dplyr::select(month, prop)
    #   mriv
    #   if(i==1){
    #     mrv <- mriv
    #   }else{
    #     mrv <- left_join(mrv, mriv, by='month')
    #   }
    #   mrv
    #   names(mrv) <- gsub('prop',as.character(eventi),names(mrv))
    #   mrv
    # }
    # (share_months <- mrv %>% as.data.frame)
    #
    # # Plot it
    # melted <- data.table::melt(share_months, id.vars=1)
    #
    # ggshare_months <-
    #   ggplot(melted, aes(y=factor(month), x=value)) +
    #   geom_col(fill='darkblue', alpha=.7) +
    #   scale_y_discrete(limits=factor(12:1)) +
    #   xlab('Percent share of predicted outcomes') +
    #   ylab('Month') +
    #   facet_wrap(~variable, scales='fixed') +
    #   labs(title = 'Share of risk by calendar month')
    #
    # MR$month <- list(table = share_months, plot = ggshare_months)


    # Share diel ===================================================================

    if(verbose){message('Calculating share of risks by diel period ...')}

    mrii <-
      mri %>%
      group_by(diel, event, iteration) %>%
      summarize(outcome = sum(outcome)) %>%
      group_by(diel, event) %>%
      summarize(mean = mean(outcome),
                median = median(outcome),
                q5 = quantile(outcome, .05),
                q95 = quantile(outcome, .95),
                q20 = quantile(outcome, .2)) %>%
      group_by(event) %>%
      mutate(tot = sum(mean)) %>%
      ungroup() %>%
      mutate(prop = round(100*(mean / tot))) %>%
      mutate(prop = ifelse(!is.finite(prop), 0, prop)) %>%
      group_by(event, diel) %>%
      summarize(prop = prop)

    MR$diel <- mrii

#
#     mri <-
#       mrm %>%
#       group_by(event, month, channel, vessel, diel) %>%
#       summarize(iteration = 1:1000,
#                 outcome = outcome[sample(1:n(),size=1000, replace=TRUE)])
#
#     mri %>% nrow
#     mri %>% data.frame %>% head(100)
#
#     mrii <-
#       mri %>%
#       group_by(diel, event, iteration) %>%
#       summarize(outcome = sum(outcome)) %>%
#       group_by(diel, event) %>%
#       summarize(mean = mean(outcome),
#                 median = median(outcome),
#                 q5 = quantile(outcome, .05),
#                 q95 = quantile(outcome, .95),
#                 q20 = quantile(outcome, .2))
#     mrii
#
#     mriii <-
#       mrii %>%
#       group_by(event) %>%
#       mutate(tot = sum(mean)) %>%
#       ungroup() %>%
#       mutate(prop = round(100*(mean / tot))) %>%
#       mutate(prop = ifelse(!is.finite(prop), 0, prop))
#     mriii
#
#     (events <- unique(mrii$event))
#     i=1
#     for(i in 1:length(events)){
#       (eventi <- events[i])
#       mriv <-
#         mriii %>%
#         dplyr::filter(event == eventi) %>%
#         dplyr::select(diel, prop)
#       mriv
#       if(i==1){
#         mrv <- mriv
#       }else{
#         mrv <- left_join(mrv, mriv, by='diel')
#       }
#       mrv
#       names(mrv) <- gsub('prop',as.character(eventi),names(mrv))
#       mrv
#     }
#
#     (share_diel <- mrv %>% as.data.frame)
#
#     # Plot it
#     melted <- data.table::melt(share_diel, id.vars=1)
#
#     ggshare_diel <-
#       ggplot(melted, aes(y=factor(diel), x=value)) +
#       geom_col(fill='darkblue', alpha=.7) +
#       xlab('Percent share of predicted outcomes') +
#       ylab('Diel period') +
#       facet_wrap(~variable, scales='fixed') +
#       labs(title = 'Share of risk by night vs. day')
#
#     MR$diel <- list(table = share_diel, plot = ggshare_diel)
    MR$diel <- mrii

  })

  return(MR)

}

