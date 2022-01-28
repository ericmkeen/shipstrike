#' Tally encounters
#'
#' This includes processing near misses
#'
#' @param encounters Result of `encounter_simulator()`
#'
#' @return
#' @export
#'
encounter_tally <- function(encounters){

  #encounters
  mri <- encounters$summary

  # Note number of imminent encounters that occurred in this iteration
  (encounters <- which(mri$encounter == 1))
  (tot_encounters <- length(encounters))

  # Add identifier for each row
  mri$id <- 1:nrow(mri)

  # Find position of the whale's fluke
  mri$whale_dx <- cos(2*pi - (mri$whale_hdg*pi/180) + pi/2)*(mri$whale_l)
  mri$whale_x_fluke <- mri$whale_x + mri$whale_dx

  # Describe where along the length of the ship the whale is occurring
  mri$ship_frac <- (mri$ship_y_bow - mri$whale_y) / (mri$ship_y_bow - mri$ship_y_stern)

  # Find the near misses
  near_misses <- which(mri$encounter==0 & mri$closest < mri$whale_l)
  (misses <- mri[near_misses,])
  nrow(misses)

  if(nrow(misses)>0){
    # Inspect these near misses to see if they should count
    hit <- rep(FALSE,nrow(misses)) # Stage results vector
    for(i in 1:nrow(misses)){ # Loop through each near-miss
      missi <- misses[i,]

      # Determine ship radius for this near-miss
      ship_rad <- round(missi$ship_w / 2)  ; ship_rad

      # Does the whale's fluke occur within the cross-sectional width of the ship?
      hit_test <- round(missi$whale_x_fluke) %in% (-1*ship_rad):ship_rad ; hit_test

      # Is the whale occurring between the bow and the stern?
      y_test <- round(missi$whale_y) %in% round(missi$ship_y_bow):round(missi$ship_y_stern); y_test

      # If both are true, this is actually an encounter
      if(all(c(hit_test,y_test))){hit[i] <- TRUE}
    }

    # Based on these results, update the number of encounters
    (hits <- misses[hit,])
    (to_change <- which(mri$id %in% hits$id))
    mri$encounter[to_change] <- 1

    # Update number of encounters
    encounters
    (encounters <- which(mri$encounter == 1))
    tot_encounters
    (tot_encounters <- length(encounters))
  }

  return(list(total = tot_encounters,
              details = mri[encounters,],
              whale_length = mri %>% dplyr::filter(closest < whale_l)))

}
