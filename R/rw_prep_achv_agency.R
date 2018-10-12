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
  
  df_all <- df %>% 
    rw_prep_achv(indicator) %>%
    dplyr::select(indicator, achievement) %>%
    dplyr::arrange(achievement) %>%
    dplyr::full_join(., df_agency, by = "indicator")
  
  df_label <- df_all %>% 
    dplyr::select(-achievement) %>% 
    tidyr::gather(agency, pct, CDC, USAID) %>% 
    dplyr::filter(pct !=0, !is.na(pct)) %>% 
    dplyr::group_by(agency) %>% 
    dplyr::slice(1) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(lab_CDC = ifelse(agency == "CDC", 1, NA),
                  lab_USAID = ifelse(agency == "USAID", 1, NA)) %>% 
    dplyr::select(-c(agency, pct))
  
  if(dplyr::n_distinct(df_label$indicator) == 1){
    df_label <- df_label %>% 
      dplyr::mutate(lab_USAID = 1) %>% 
      dplyr::slice(1)
  }
  
  dplyr::left_join(df_all, df_label, by = "indicator")

}

