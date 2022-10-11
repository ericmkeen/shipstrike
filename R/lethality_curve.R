#' Produce a P(Lethality) ~ Speed curve
#'
#' This function returns the Probability of Lethality for a range of speeds based on
#' a logistic function of form: `P(Lethality) = asymptote/ (1 + exp(-1*(c1 + (c2*speeds))))`.
#' The defaults are the values used in *Keen et al. (2023)* for large ships > 180m.
#'
#' @param speeds  A vector of speeds at which to predict P(Lethality).
#' @param c1  First parameter
#' @param c2  Second parameter
#' @param asymptote Upper asymptote of the function.
#'
#' @return A `data.frame`.
#' @export
#'
lethality_curve <- function(speeds = seq(0, 30, length=1000),
                            c1 = -1.241241,
                            c2 = 0.2712432,
                            asymptote=1.0){
  morts <- (asymptote/ (1 + exp(-1*(c1 + (c2*speeds)))))
  dfi <- data.frame(speed = speeds, Plethality = morts)
  return(dfi)
}
