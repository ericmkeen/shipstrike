#' Simulate whale density (mean / CV)
#'
#' A simple function that creates a spatial grid of whale densities
#' for use in simulating a shipping impacts assessment.
#'
#' @param grids The spatial grid to use as the basis for the density surface,
#' as produced by `shipstrike::make_grid()` (see documentation).
#' @param d_mean The mean density (animals per square km)
#' @param d_cv The co-efficient of variation (CV) in density.
#' @param iterations Number of bootstrap iterations to produce.
#'
#' @return The `grids` object, with new columns `D` and `iteration` indicating an iteration number.
#' @export
#'
simulate_whale <- function(grids,
                           d_mean = 0.01,
                           d_cv = 0.5,
                           iterations = 1000){
  d_sd <- d_mean*d_cv
  df <- data.frame(grid_id= rep(grids$id, each=iterations),
                   iteration = rep(1:iterations, times=nrow(grids)),
                   D = truncnorm::rtruncnorm(n = nrow(grids)*iterations,
                                  a = 0,
                                  b = Inf,
                                  mean = d_mean,
                                  sd = d_sd))

  return(df)
}
