#' Simulate whale density (mean / CV)
#'
#' @param grids desc
#' @param d_mean desc
#' @param d_cv desc
#'
#' @return
#' @export
#'
simulate_whale <- function(grids,
                           d_mean = 0.001,
                           d_cv = 0.3){
  grids$d_mean <- d_mean
  grids$d_cv <- d_cv
  return(grids)
}
