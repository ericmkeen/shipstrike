#' Core function for whale-ship collision outcome
#'
#' @param p_whale desc
#' @param p_encounter desc
#' @param p_surface desc
#' @param p_avoidance desc
#' @param p_lethality desc
#' @param km desc
#' @param interations desc
#' @param verbose desc
#'
#' @return
#' @export
#'
outcome <- function(p_whale,
                    p_encounter,
                    p_surface,
                    p_avoidance,
                    p_lethality,
                    km,
                    iterations=1000,
                    verbose=TRUE){

  # Core function: results for one value for each parameter, for a single cell

  if(FALSE){# debugging
    p_whale = 0.05; p_encounter = 0.5; p_surface = 0.5; p_avoidance = 0.10; p_lethality = 0.50
    km = 12000
    iterations = 10000
    verbose = TRUE
  }

  suppressWarnings({
    suppressMessages({

      if(verbose & iterations > 1){pb <- txtProgressBar(1, iterations, style=3)}
      results <- data.frame()
      for(i in 1:iterations){
        # =============================================================

        # Co-occurrences
        if(length(p_whale)>1){
          ps = sample(p_whale, km, TRUE)
        }else{
          ps = rep(p_whale, times= km)
        }
        runifs <- runif(km, 0, 1)
        verdicts <- runifs < ps
        verdicts[! verdicts] <- 0
        verdicts[verdicts] <- 1
        dfi <- data.frame(cooccurrence = verdicts %>% as.numeric)
        apply(dfi, 2, sum)

        # Close encounters
        if(length(p_whale)>1){
          ps = sample(p_encounter, km, TRUE)
        }else{
          ps = rep(p_encounter, times= km)
        }
        runifs <- runif(km, 0, 1)
        verdicts <- runifs < ps & dfi$cooccurrence == 1
        verdicts[! verdicts] <- 0
        verdicts[verdicts] <- 1
        dfi$encounter <- verdicts %>% as.numeric
        apply(dfi, 2, sum)

        # Surface overlap
        if(length(p_whale)>1){
          ps = sample(p_surface, km, TRUE)
        }else{
          ps = rep(p_surface, times= km)
        }
        runifs <- runif(km, 0, 1)
        verdicts <- runifs < ps & dfi$encounter == 1
        verdicts[! verdicts] <- 0
        verdicts[verdicts] <- 1
        dfi$surface <- verdicts %>% as.numeric
        apply(dfi, 2, sum)

        # Avoidance
        if(length(p_whale)>1){
          ps = sample((1 - p_avoidance), km, TRUE)
        }else{
          ps = rep((1 - p_avoidance), times= km)
        }
        runifs <- runif(km, 0, 1)
        verdicts <- runifs < ps & dfi$surface == 1
        verdicts[! verdicts] <- 0
        verdicts[verdicts] <- 1
        dfi$collision <- verdicts %>% as.numeric
        apply(dfi, 2, sum)

        # Lethality
        if(length(p_whale)>1){
          ps = sample(p_lethality, km, TRUE)
        }else{
          ps = rep(p_lethality, times= km)
        }
        runifs <- runif(km, 0, 1)
        verdicts <- runifs < ps & dfi$collision == 1
        verdicts[! verdicts] <- 0
        verdicts[verdicts] <- 1
        dfi$mortality <- verdicts %>% as.numeric
        apply(dfi, 2, sum)

        dfi %>% head
        result <- apply(dfi, 2, sum) %>% as.data.frame(col.names=names(dfi)) %>% t
        result <- result %>% as.data.frame

        # =============================================================
        results <- rbind(results, result)
        if(verbose & iterations > 1){setTxtProgressBar(pb, i)}
      }

    })
  })
  return(results)
}
