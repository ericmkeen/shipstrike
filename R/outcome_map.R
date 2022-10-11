#' Plot map of outcomes
#'
#' This function produces a map of a risk metric.
#' The default is to show cooccurrences for all month-diel-vessels.
#'
#' @param outcomes_grid  The `outcomes_grid` output from `outcome_predict()`.
#' @param base_map  Base map, provided as a `ggplot2` object. The default is to
#' use `bangarang::gg_kfs()` of the Kitimat Fjord System (BC, Canada).
#' @param varplot  The variable to plot. See output of `outcome_predict()`.
#' @param varfacet The variable by which to facet, if any.
#' @param spp  Species to filter by.
#' @param years  Year to filter by.
#' @param channels  `channel` to filter by.
#' @param vessels  `vessels` to filter by.
#' @param months  `months` (numerics) to filter by.
#' @param diels  Diel period `"night"` or `"day"`).
#' @param col_min  Specify minimum color of color gradient scale.
#' @param col_max  Specify maximum color of color gradient scale.
#'
#' @return A `ggplot` object.
#' @export
#' @import dplyr
#' @import ggplot2
#'
outcome_map <- function(outcomes_grid,
                        base_map = NULL,
                        varplot = 'cooccurrence',
                        varfacet = NULL,
                        spp = NULL,
                        years = NULL, channels = NULL,
                        vessels = NULL,
                        months = NULL,
                        diels = NULL,
                        col_min = 'white',
                        col_max = 'darkorange4'){

  if(FALSE){
    outcomes_grid <- readRDS('tests/fw/impacts_lng_canada/8_14_knots/outcomes_grid.RData')
    head(outcomes_grid)
    nrow(outcomes_grid)

    spp <- years <- vessels <- channels <- vessels <- months <- diels <- NULL
    varplot <- 'cooccurrence'
    varfacet <- 'vessel'
    col_min = 'white'
    col_max = 'darkorange4'

    outcome_map(outcomes_grid)
    outcome_map(outcomes_grid, varplot='collision2.2')
    outcome_map(outcomes_grid, varfacet = 'vessel')
    outcome_map(outcomes_grid, varplot='collision2.2', varfacet = 'vessel')
  }

  #=============================================================================
  # Summarizing outcomes so that there is a single row per grid cell

  mr <- outcomes_grid
  head(mr)

  data(grid_kfs)
  names(grid_kfs)
  grid2join <- grid_kfs %>% select(grid_id = id, x, y)

  mr <- left_join(mr, grid2join, by='grid_id')
  names(mr)

  (vals <- mr %>% pull(which(names(mr)==varplot)) %>% as.numeric)
  mr$vals <- vals

  if(is.null(varfacet)){
    mr$faceter <- 'All'
  }else{
    (faceter <- mr %>% pull(which(names(mr)==varfacet)))
    mr$faceter <- faceter
  }

  if(!is.null(spp)){ mr <- mr %>% filter(species %in% spp) }
  if(!is.null(years)){ mr <- mr %>% filter(year %in% years) }
  if(!is.null(vessels)){ mr <- mr %>% filter(vessel %in% vessels) }
  if(!is.null(channels)){ mr <- mr %>% filter(channel %in% channels) }
  if(!is.null(months)){ mr <- mr %>% filter(month %in% months) }
  if(!is.null(diels)){ mr <- mr %>% filter(diel %in% diels) }

  mrs <-
    mr %>%
    mutate(sumout = sum(vals)) %>%
    group_by(faceter, grid_id) %>%
    summarize(x = x[1], y = y[1],
              outcomes = sum(vals),
              sumout = sumout[1]) %>%
    mutate(outcomes = outcomes / sumout)

  nrow(mrs)

  #=============================================================================
  # Preparing base map

  if(is.null(base_map)){
    base_map <- bangarang::gg_kfs()
  }

  gg <-
    base_map +
    geom_raster(data=mrs,
               mapping=aes(x=x, y=y, fill=outcomes)) +
    scale_fill_gradient2(
      low = col_min,
      mid = col_min,
      high = col_max) +
    labs(fill = 'Share of risk') +
    xlab(NULL) + ylab(NULL)

  if(!is.null(varfacet)){
    gg <-
      gg +
      facet_wrap(~faceter) +
      ggplot2::theme(strip.background = ggplot2::element_rect(fill="grey50"))
  }

  gg

  gg <- gginnards::move_layers(gg, idx=1, position='top')

  return(gg)
}
