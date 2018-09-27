#' Calcuate achievement
#'
#' @param df dataframe to add achievement onto
#'
#' @export

rw_calc_achievement <- function(df){
  
  #calculate achievement
    df_agg <- dplyr::mutate(df, achievement = round(fy2018cum/fy2018_targets, 2))
  
  #TODO make calculation dynamic
    
  #remove errors (eg denom = 0 -> Inf)
    df_agg <- dplyr::mutate(df_agg, achievement = ifelse(is.finite(achievement), achievement, NA))
  
  #order ascending
    df_agg <- dplyr::arrange(df_agg, achievement)
    
  return(df_agg)
}