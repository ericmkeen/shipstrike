#' Produce ellipses and test for close encounters
#'
#' This is an internal `shipstrike` function not called directly by users.
#'
#' @param transits A list of transit details produced during the `encounter_rate()` code.
#'
#' @return A `list`.
#' @export
#' @import dplyr
#'
transit_ellipses <- function(transits){

  transits %>% head
  transits %>% names

  (tw_names <<- names(transits))

  # Split into a list by id_ship
  transit_list <- transits %>%
    group_by(id_ship) %>%
    group_split

  #=============================================================================
  # Convert each group into ellipses

  ellipses <- lapply(transit_list, function(tw){

    # Create a set of whale ellipse polygons
    ewhales <<-
      apply(tw, 1, function(twi){
        names(twi) <- tw_names
        ellipse(x=as.numeric(as.character(twi[which(tw_names == 'x_rostrum')])),
                y=as.numeric(as.character(twi[which(tw_names == 'y_rostrum')])),
                width=as.numeric(as.character(twi[which(tw_names == 'w.whale')])),
                height=as.numeric(as.character(twi[which(tw_names == 'l.whale')])),
                theta=as.numeric(as.character(twi[which(tw_names == 'hdg_whale')])),
                plot=F)$coords %>%
          st_as_sf(coords = c("x", "y")) %>%
          summarise(geometry = st_combine(geometry)) %>%
          st_cast("POLYGON")
      })
    #ewhales

    # Create a set of ship ellipse polygons
    eships <<-
      apply(tw, 1, function(twi){
        names(twi) <- tw_names
        ellipse(x=as.numeric(as.character(twi[which(tw_names == 'x_ship')])),
                y=as.numeric(as.character(twi[which(tw_names == 'y_center')])),
                width=as.numeric(as.character(twi[which(tw_names == 'w.ship')])),
                height=as.numeric(as.character(twi[which(tw_names == 'l.ship')])),
                theta=0,
                plot=F)$coords %>%
          st_as_sf(coords = c("x", "y")) %>%
          summarise(geometry = st_combine(geometry)) %>%
          st_cast("POLYGON")
      })
    #eships

    nearest <- mapply(st_distance, ewhales, eships)

    return(list(whale = ewhales, ship = eships, nearest = nearest))
  })

  #=============================================================================

  ellipses %>% length
  ellipses[[1]] %>% names
  ellipses[[1]]$nearest

  transits$nearest <- lapply(ellipses,'[[',3) %>% unlist
  head(transits)

  # Determine encounters
  transits <-
    transits %>%
    mutate(encounter = ifelse(nearest == 0, TRUE, FALSE))

  # Review
  transits$danger_zone %>% table
  transits$encounter %>% table

  # Tally encounters
  encounter_transits <-
    transits %>%
    filter(encounter == TRUE) %>%
    pull(id_ship) %>%
    unique

  return_list <- list(transits = transits,
                      ellipses = ellipses,
                      encounter_transits = encounter_transits,
                      n_encounters = length(encounter_transits))

  return(return_list)
}

