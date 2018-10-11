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
    dplyr::filter(fundingagency == "USAID",
                  standardizeddisaggregate == "Total Numerator") %>% 
    rw_summarize(mechanismid, indicator)
  
  #identify current year
  curr_fy <- ICPIutilities::identifypd(df, pd_type = "year")
  
  #add column for achievement
  df <- rw_calc_achievement(df, curr_fy)
  
  #filter, keeping only under performancing partners
  df <- df %>% 
    dplyr::filter(achievement < threshold) %>% 
    dplyr::arrange(mechanismid, indicator) 
  
  #identify current year cum and targets
  curr_cum <- paste0("fy", curr_fy, "cum")
  curr_targets <- ICPIutilities::identifypd(df, pd_type = "target")
  df <- df %>% 
    dplyr::select(mechanismid, indicator, curr_cum, curr_targets, achievement)
  
  return(df)
}
