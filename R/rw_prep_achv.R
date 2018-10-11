#' Create a table for graphing achievement
#'
#' @param df dataframe to 
#'
#' @importFrom dplyr %>% 
#' @export


rw_prep_achv <- function(df, ...){
  
  #capture grouping variables as a list of formulas
    group_vars <- dplyr::quos(...)
    
  #identify current year cum and targets
    curr_fy <- ICPIutilities::identifypd(df, pd_type = "year")
    curr_cum <- paste0("fy", curr_fy, "cum")
    curr_targets <- ICPIutilities::identifypd(df, pd_type = "target")
    
  #generate a table of cumulative results and targets for all indicators
    df %>% 
      dplyr::filter(standardizeddisaggregate == "Total Numerator") %>%
      dplyr::group_by(!!!group_vars) %>%
      dplyr::summarise_at(dplyr::vars(curr_cum, curr_targets), ~ sum(., na.rm = TRUE)) %>%
      dplyr::ungroup() %>% 
      #calculate achievement
      rw_calc_achievement() %>% 
      dplyr::arrange(achievement)
}
