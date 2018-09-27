#' Extract Threshold
#'
#' @param quarter current fiscal quarter
#' @param lvl "low"; "med", "good"
#'
#' @importFrom dplyr %>%
#' @export
#'

rw_pull_threshold <- function(quarter, lvl){
  
  #quarterly thresholds
    thres_table <- 
      tibble::tribble(~qtr, ~low, ~med, ~good,
                        1L, 0.01, 0.15,  0.35,
                        2L, 0.25,  0.4,   0.6,
                        3L,  0.5, 0.65,  0.85,
                        4L, 0.75,  0.9,   1.1)
  
  #extract value based on quarter and value
    thres_table %>% 
      dplyr::filter(qtr == quarter) %>% 
      dplyr::select(lvl) %>% 
      dplyr::pull(lvl)
  
}