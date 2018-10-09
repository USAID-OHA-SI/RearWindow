#' Identify underperforming partners
#'
#' @param df dataframe to use, needs to contain cumulative and targets
#' @param threshold what level counts as under achievement? eg Q3 = 0.65
#'
#' @importFrom dplyr %>%
#' @export


rw_identify <- function(df, threshold){
  
  #limit to just USAID partner & clean up by aggregating prior to achievement 
  df <- df %>%
    dplyr::filter(fundingagency == "USAID") %>% 
    rw_summarize(mechanismid, indicator)
  
  #add column for achievement
  df <- rw_calc_achievement(df)
  
  #filter, keeping only under performancing partners
  df <- df %>% 
    dplyr::filter(achievement < threshold) %>% 
    dplyr::arrange(mechanismid, indicator)
  
  return(df)
}
