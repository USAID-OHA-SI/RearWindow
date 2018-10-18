
#' Prep table for achievement by indicator and mechanism
#'
#' @param df dataframe to use
#'
#' @export
#' @importFrom dplyr %>% 

rw_prep_mech_table <- function(df){
  
  #setup table of achievement by each mechanism's indicator
  df %>% 
    #add achievement for current pd & drop mech/inds w/o achievement
    rw_identify() %>% 
    dplyr::select(mechanismid, indicator, achievement)  %>% 
    dplyr::filter(!is.na(achievement)) %>% 
    #create a ranking for mechanism for the graph by # of ind reported on
    dplyr::group_by(mechanismid) %>% 
    dplyr::mutate(ind_count = n()) %>% 
    dplyr::ungroup() %>% 
    dplyr::arrange(desc(indicator)) %>% 
    #adjust indicator to be factor for ordering purposes
    dplyr::mutate(indicator = forcats::as_factor(indicator)) %>%
    dplyr::arrange(desc(ind_count), mechanismid, indicator)
}

  