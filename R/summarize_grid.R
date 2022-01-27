#' Summarize vessel grid by type, month, diel, and grid_id
#'
#' @param vgrid desc
#'
#' @return
#' @export
#'
summarize_grid <- function(vgrid){

  head(vgrid)
  vgrid$month <- lubridate::month(vgrid$datetime)

  vsumm <-
    vgrid %>%
    dplyr::group_by(type, month, diel, grid_id) %>%
    dplyr::summarize(x = x[1],
                     y= y[1],
                     n = dplyr::n(),
                     #vid = paste(vid, collapse='_'),
                     speed = paste(round(speed,1), collapse='_'),
                     length = paste(round(length,1), collapse='_'),
                     width = paste(round(width,1), collapse='_'),
                     draft = paste(round(draft,1), collapse='_'))

  return(vsumm)
}
