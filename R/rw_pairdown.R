#' Indicator selection
#'
#' @param df data frame to review
#' @param opunit operatingunit
#' @importFrom dplyr %>%
#' @export


rw_pairdown <- function(df, opunit){
  
  #filter to selected OU
    df <- dplyr::filter(df, operatingunit == opunit)
  
  #identify current period
    curr_pd <- ICPIutilities::identifypd(df)
    
  #identify Q3 indicators
    ind_list <- df %>%
      dplyr::filter(standardizeddisaggregate == "Total Numerator") %>%
      dplyr::group_by(indicator) %>%
      dplyr::summarise_at(dplyr::vars(curr_pd), ~ sum(., na.rm = TRUE)) %>%
      dplyr::ungroup() %>%
      dplyr::filter_at(dplyr::vars(curr_pd), dplyr::any_vars(.!= 0)) %>% 
      dplyr::filter(!indicator %in% c("PMTCT_EID_Less_Equal_Two_Months", "PMTCT_EID_Two_Twelve_Months",
                               "PMTCT_STAT_KnownatEntry_POSITIVE", "PMTCT_STAT_NewlyIdentified_Negative",
                               "PMTCT_STAT_NewlyIdentified_POSITIVE", "PMTCT_HEI_POS", "HTS_TST_NEG")) %>%
      dplyr::pull(indicator)
    
  #filter df to include indicators reported in pd and only numerator
    df <- dplyr::filter(df, 
                        indicator %in% ind_list,
                        standardizeddisaggregate == "Total Numerator")
}