#' Plot diagnostics for a single encounter-rate transit
#'
#' @param transits The result of `encounter_rate()` in which `runs` is set to `1`.
#' @param id The iteration ID for which you want a detailed diagnostic plot. Can be
#' left `NULL`, which will begin at the first iteration then step through every iteration
#' in the `transits` object until you press Escape.
#'
#' @param wait Boolean, with default `TRUE`, indicating if you want `R` to wait for you
#' to press `Enter` between each iteration, or just move automatically through the entire
#' `transits` object.
#'
#' @return A series of diagnostic plots depicting the details of a close-encounter simulation iteration.
#'
#' @export
#' @import dplyr
#' @import ggplot2
#' @import ggpubr
#' @import sf
#'
encounter_diagnostics <- function(transits,
                              id = NULL,
                              wait=TRUE){

  #transits <- mri

  # Stage list of results
  ggall <- list()

  if(is.null(id)){
    ids <- unique(transits$id_ship)
  }else{
    ids <- id
  }

  transits$encounter[is.na(transits$encounter)] <- FALSE

  encs <- transits %>%
    filter(encounter == TRUE) %>%
    pull(id_ship) %>%
    unique

  message('finding ',length(encs),' transit(s) with encounters: ',
          paste(encs, collapse=', '))

  i=6
  for(i in 1:length(ids)){
    (idi <- ids[i])

    # Filter to this transit
    tw <- transits %>% filter(id_ship == idi)
    names(tw)

    if(any(tw$encounter)){
      message('\nTransit ', idi,' : Encounter occurs!!! ***')
      plot_tit <- paste0('Transit ID ',idi,' : close encounter!')
    }else{
      message('\nTransit ', idi,' : no encounter.')
      plot_tit <- paste0('Transit ID ',idi)
    }

    message('\nAnalyzing ellipses ...')
    transit_results <- transit_ellipses(tw)

    # Retrieve and parse ellipses
    ellipses <- transit_results$ellipses
    ewhales <- lapply(ellipses,'[[','whale')[[1]]
    eships <- lapply(ellipses,'[[','ship')[[1]]

    # Retrieve updated transit data
    tw <- transit_results$transits

    # Timeline plot ================================================================
    message('Creating timeline plot ...')

    # Find timestamp of nearest point
    (neari <- which.min(tw$nearest))
    neari

    gg_timeline <-
      ggplot(tw, aes(x=t, y=nearest)) +
      geom_point() +
      theme_light() +
      xlab('Simulation timestamp') +
      ylab('Distance between whale & ship') +
      ylim(0, 1200) +
      geom_point(data=tw, mapping=aes(x=t[neari], y=nearest[neari]), color='firebrick', size=4, alpha=.4) +
      annotate('text', x=neari, y=tw$nearest[neari] + 100, label='First\nclosest\npoint') +
      labs(title=plot_tit)


    # Create map  ================================================================
    message('Creating map ...')

    R <- 564.3185
    arena <-
      data.frame(x=0, y=0) %>%
      sf::st_as_sf(coords=c('x','y')) %>%
      sf::st_buffer(dist=R)

    gg <-
      ggplot(arena) +
      geom_sf(alpha=.3) +
      theme_light() +
      geom_vline(xintercept = tw$x_port[1], lty=3, color='darkblue', alpha=.5) +
      geom_vline(xintercept = tw$x_star[1], lty=3, color='darkblue', alpha=.5) +
      xlab(NULL) + ylab(NULL)

    # Assign a color to t
    gg_arena <-
      gg +
      geom_point(data=tw, mapping=aes(x=x_ship, y=y_bow, color = t),
                 alpha=.5, size=.8) +
      geom_point(data=tw, mapping=aes(x=x_rostrum, y=y_rostrum, color = t),
                 alpha=.5, size=.8) +
      labs(color='Timestamp') +
      # Whale at start:
      geom_sf(data=ewhales[[1]], color='black') +
      # Whale at closest point
      geom_point(data=tw, mapping=aes(x=x_rostrum[neari], y=y_rostrum[neari]),
                 color='firebrick', size=.8, alpha=1) +
      geom_sf(data=ewhales[[neari]], color='firebrick') +
      # Whale at end:
      geom_sf(data=ewhales[[length(ewhales)]], color='dodgerblue') +
      # Ship at start:
      geom_sf(data=eships[[1]], fill='black', alpha=.5, color=NA) +
      # Ship at closest point
      geom_point(data=tw, mapping=aes(x=x_ship[neari], y=y_bow[neari]),
                 color='firebrick', size=.8, alpha=1) +
      geom_sf(data=eships[[neari]], fill='firebrick', alpha=.4) +
      # Ship 30 seconds prior
      geom_sf(data=eships[[max(c(1,(neari-30)))]], fill='firebrick', color=NA, alpha=.3) +
      # Ship 30 seconds after
      geom_sf(data=eships[[min(c(length(eships),(neari+30)))]], fill='firebrick', color=NA, alpha=.3) +
      # Ship at end:
      geom_sf(data=eships[[length(ewhales)]], fill='dodgerblue', color=NA, alpha=.4)

    gg <- ggpubr::ggarrange(gg_timeline, gg_arena)
    print(gg)
    if(i < length(ids)){
      if(wait){
        readline(prompt="Press [enter] to continue")
      }
    }

    # Add plot to list of results
    ggall[[length(ggall) + 1]] <- gg
  } # end of loop

  return(ggall)
}

#
