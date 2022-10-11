#' Produce a P(Collision) ~ Speed curve
#'
#' This function returns the Probability of Collision for a range of speeds based on
#' a logistic function of form: `P(Collision) = asymptote/ (1 + exp(c1*(speeds - c2)))`.
#' The defaults are the values used in *Keen et al. (2023)* for large ships > 180m.
#'
#' @param speeds  A vector of speeds at which to predict P(Lethality).
#' @param c1  First parameter.
#' @param c2  Second parameter.
#' @param asymptote Upper asymptote of the function.
#'
#' @return A `data.frame`.
#' @export
#'
collision_curve<- function(speeds = seq(0, 30, length=1000),
                           c1 = -.2002002,
                           c2 = 11.80241,
                           asymptote = .9){
  avoids <- (asymptote/ (1 + exp(c1*(speeds - c2))))
  dfi <- data.frame(speed = speeds, Pcollision = avoids)
  return(dfi)
}
