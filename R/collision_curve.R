#' Produce a P(Collision) ~ Speed curve
#'
#' @param speeds  desc
#' @param c1  desc
#' @param c2  desc
#' @param asymptote desc
#'
#' @return
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
