#' Simulate whale density (mean / CV)
#'
#' A simple function that creates a spatial grid of whale densities
#' for use in simulating a shipping impacts assessment.
#'
#' @param grids The spatial grid to use as the basis for the density surface,
#' as produced by `shipstrike::make_grid()` (see documentation).
#' @param d_mean The mean density (animals per square km)
#' @param d_cv The co-efficient of variation (CV) in density.
#'
#' @return The `grids` object, with new columns `d_mean` and `d_cv`.
#' @export
#'
simulate_whale <- function(grids,
                           d_mean = 0.001,
                           d_cv = 0.3){
  grids$d_mean <- d_mean
  grids$d_cv <- d_cv
  return(grids)
}
