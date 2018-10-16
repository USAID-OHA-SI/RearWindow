#' Extract Threshold Values
#'
#' @param qtr current fiscal quarter, eg "q2"
#'
#' @export


rw_addthresholds <- function(qtr){
  
  #table of thresholds values
    thres_table <- 
      tibble::tribble(
        ~threshold,  ~q1,  ~q2,  ~q3,  ~q4,
             "low", 0.01, 0.25,  0.5, 0.75,
             "med", 0.15,  0.4, 0.65,  0.9,
            "good", 0.35,  0.6, 0.85,  1.1,
             "ach",    1,    1,    1,    1
        )

  #extract thresholds as vector
    thres <- dplyr::pull(thres_table, qtr)
  
  #apply names to each threshold (named vector)
    names(thres) <- dplyr::pull(thres_table, threshold)
  
  return(thres)
}