#' Calcuate achievement
#'
#' @param df dataframe to add achievement onto
#' @param curr_fy current fiscal year, eg 2018L
#' 
#' @importFrom stats setNames
#' @export

rw_calc_achievement <- function(df, curr_fy){
  
  #identify current year cum and targets
    curr_cum <- paste0("fy", curr_fy, "cum")
    curr_targets <- paste0("fy", curr_fy, "_targets")
  
  #setup for calculating achievement with mutate_
    var_name <- "achievement"
    fcn <- paste0("round(", curr_cum, "/", curr_targets, ", 2)")
  
  #calculate achievement
    df_agg <- dplyr::mutate_(df, .dots = setNames(fcn, var_name))
    
  #remove errors (eg denom = 0 -> Inf)
    df_agg <- dplyr::mutate(df_agg, achievement = ifelse(is.finite(achievement), achievement, NA))
  
  #order ascending
    df_agg <- dplyr::arrange(df_agg, achievement)
    
  return(df_agg)
}