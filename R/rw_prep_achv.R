#' Create a table for graphing achievement
#'
#' @param df dataframe to 
#' @param ... grouping variable, eg indicator
#' @param ind can specify if you want to look at a disagg of one indicator, default = NULL
#'
#' @importFrom dplyr %>% 
#' @export


rw_prep_achv <- function(df, ..., ind = NULL){
  
  #capture grouping variables as a list of formulas
    group_vars <- dplyr::quos(...)
    
  #identify current year cum and targets
    curr_fy <- ICPIutilities::identifypd(df, pd_type = "year")
    curr_cum <- paste0("fy", curr_fy, "cum")
    curr_targets <- ICPIutilities::identifypd(df, pd_type = "target")
  
  #If ind = NULL, comparing all indicators; otherwise, comparinga disagg for one variable
    if(is.null(ind)){
      df <- dplyr::filter(df, standardizeddisaggregate == "Total Numerator")
    } else {
      df <- dplyr::filter(df, indicator == ind, standardizeddisaggregate != "Total Numerator")
    }
    
  #generate a table of cumulative results and targets for all indicators
    df %>% 
      dplyr::group_by(!!!group_vars) %>%
      dplyr::summarise_at(dplyr::vars(curr_cum, curr_targets), ~ sum(., na.rm = TRUE)) %>%
      dplyr::ungroup() %>% 
      #calculate achievement
      rw_calc_achievement() %>% 
      dplyr::arrange(achievement)
}
