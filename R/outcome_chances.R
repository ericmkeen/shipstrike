#' Chances of at least / no more than X interactions
#'
#' @param outcomes desc
#' @param collision_var desc
#' @param mortality_Var desc
#'
#' @return desc
#' @export
#' @import dplyr
#'
outcome_chances <- function(outcomes,
                            collision_var = 'collision2.2',
                            mortality_var = 'mortality2.2'){
  #collision_var = 'collision2.2'
  #mortality_var = 'mortality2.2'

  suppressMessages({
    mrs <-
    outcomes %>%
    group_by(iteration) %>%
    summarize(across(cooccurrence:mortality2.4, sum)) %>%
    rename(collision = collision_var, #collision2.3,
           mortality = mortality_var) %>%  #mortality2.3) %>%
    dplyr::select(-iteration)
  })

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
  MR <- list(at_least = chances)

  # Chances of *no more than* X events
  m0 <- data.frame(Chances_of = 'Zero',
                   Collisions = round(100*(length(which(mrs$collision == 0))/ nrow(mrs)),1),
                   Mortalities = round(100*(length(which(mrs$mortality == 0))/ nrow(mrs)),1))
  m1 <- data.frame(Chances_of = 'Max of 1',
                   Collisions = round(100*(length(which(mrs$collision <= 1))/ nrow(mrs)),1),
                   Mortalities = round(100*(length(which(mrs$mortality <= 1))/ nrow(mrs)),1))
  m2 <- data.frame(Chances_of = 'Max of 2',
                   Collisions = round(100*(length(which(mrs$collision <= 2))/ nrow(mrs)),1),
                   Mortalities = round(100*(length(which(mrs$mortality <= 2))/ nrow(mrs)),1))
  m3 <- data.frame(Chances_of = 'Max of 3',
                   Collisions = round(100*(length(which(mrs$collision <= 3))/ nrow(mrs)),1),
                   Mortalities = round(100*(length(which(mrs$mortality <= 3))/ nrow(mrs)),1))
  m4 <- data.frame(Chances_of = 'Max of 4',
                   Collisions = round(100*(length(which(mrs$collision <= 4))/ nrow(mrs)),1),
                   Mortalities = round(100*(length(which(mrs$mortality <= 4))/ nrow(mrs)),1))
  m5 <- data.frame(Chances_of = 'Max of 5',
                   Collisions = round(100*(length(which(mrs$collision <= 5))/ nrow(mrs)),1),
                   Mortalities = round(100*(length(which(mrs$mortality <= 5))/ nrow(mrs)),1))

  chances <- rbind(m0, m1, m2, m3, m4, m5)
  MR$no_more_than <- chances

  return(MR)
}
