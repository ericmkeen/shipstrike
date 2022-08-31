#' Produce a P(Lethality) ~ Speed curve
#'
#' @param speeds  desc
#' @param c1  desc
#' @param c2  desc
#' @param asymptote desc
#'
#' @return
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
