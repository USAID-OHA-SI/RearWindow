#' Create a table for graphing achievement
#'
#' @param df dataframe to 
#'
#' @importFrom dplyr %>% 
#' @export


rw_prep_achv <- function(df){
  
  #generate a table of cumulative results and targets for all indicators
    df %>% 
      dplyr::filter(standardizeddisaggregate == "Total Numerator") %>%
      dplyr::group_by(indicator) %>%
      dplyr::summarise_at(dplyr::vars(fy2018cum, fy2018_targets), ~ sum(., na.rm = TRUE)) %>%
      dplyr::ungroup() %>% 
      #calculate achievement
      rw_calc_achievement() %>% 
      dplyr::arrange(achievement)
}
