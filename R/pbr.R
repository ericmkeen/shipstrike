#' Estimate Potential Biological Removal (PBR)
#'
#' Assuming a log-normal distribution of abundance estimates.
#'
#' @param N Abundance estimate
#' @param CV CV of abundance estimate
#' @param Rmax Maximum rate of increase. Default is that for cetaceans recommended by NMFS (2016).
#' @param Fr Recovery factor. Default is that for a depleted cetacean stock, recommended by NMFS (2016).
#' @param toplot Boolean, whether or not to plot the distribution of the abundance estimate.
#'
#' @return A simple list
#' @export
#'
pbr <- function(N, CV, Rmax=0.08, Fr=0.5, toplot=FALSE){

  (SD = 0.6*N)
  x <- rlnorm(10000, mean = log(N), sd = log(0.6*log(N)))

  if(toplot){
    hist(x,
         breaks=seq(0,1.1*max(x), by=10),
         xlim=c(0, quantile(x, 0.9)),
         main = "Log-normal distribution of N estimates")
  }

  Nmin = quantile(x, 0.20)

  pbri <- Nmin * (0.5*Rmax) * Fr

  results <- list(PBR = pbri,
                  Nmin = Nmin,
                  Rmax = Rmax,
                  Fr = Fr,
                  Nmedian = median(x))
  return(results)
}
