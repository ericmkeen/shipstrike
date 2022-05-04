#' Tabular summary of ship strike outcomes
#'
#' @param results A `data.frame` of results from the `shipstrike::outcome()` function.
#'
#' @return
#' @export
#'
outcome_table <- function(results){
  summs <- data.frame() ; i=1
  for(i in 1:ncol(results)){
    (coli <- results[,i] %>% as.data.frame)
    (coli <- coli[,1] %>% as.numeric)
    (nami <- names(results)[i])
    summi <- data.frame(impact = nami,
                        mean = mean(coli) %>% round,
                        median = median(coli) %>% round,
                        sd = sd(coli) %>% round,
                        L90 = quantile(coli, 0.05) %>% round,
                        U90 = quantile(coli, 0.95) %>% round,
                        C80 = quantile(coli, 0.20) %>% round)
    summi$mean5 <- summi$mean * 5
    summi$mean10 <- summi$mean * 10
    summs <- rbind(summs, summi)
  }

  return(summs)
}
