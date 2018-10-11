#' Prepare table for use with the agency comparison graphic
#'
#' @param df dataframe to use
#'
#' @importFrom dplyr %>% 
#' @export

rw_prep_achv_agency <- function(df){
  
  #identify current year cum and targets
  curr_fy <- ICPIutilities::identifypd(df, pd_type = "year")
  curr_cum <- paste0("fy", curr_fy, "cum")
  curr_targets <- ICPIutilities::identifypd(df, pd_type = "target")
  
  #setup table to graph
  df_agency <- df %>% 
    dplyr::filter(standardizeddisaggregate == "Total Numerator",
                  fundingagency %in% c("USAID", "HHS/CDC")) %>%
    dplyr::mutate(fundingagency = stringr::str_remove(fundingagency, "HHS/")) %>% 
    dplyr::group_by(fundingagency, indicator) %>%
    dplyr::summarise_at(dplyr::vars(curr_cum, curr_targets), ~ sum(., na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    rw_calc_achievement(curr_fy) %>%
    dplyr::filter(is.finite(achievement)) %>%
    dplyr::select(fundingagency, indicator, achievement) %>%
    tidyr::spread(fundingagency, achievement)
  
  rw_prep_achv(df, indicator) %>%
    dplyr::select(indicator, achievement) %>%
    dplyr::arrange(achievement) %>%
    dplyr::full_join(., df_agency, by = "indicator")

}

