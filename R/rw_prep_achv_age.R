#' Setup table for age graphing
#'
#' @param df dataframe to use
#' @param ind indicator whose age bands you want to graph
#' @param mechid if you want to graph a particular mechanism, eg "82818", default = NULL
#'
#' @importFrom dplyr %>%
#' @importFrom stats setNames
#' @export


rw_prep_achv_age <- function(df, ind, mechid = NULL) {
  
  #identify current year cum and targets
    curr_fy <- ICPIutilities::identifypd(df, pd_type = "year")
    curr_cum <- paste0("fy", curr_fy, "cum")
    curr_targets <- ICPIutilities::identifypd(df, pd_type = "target")
  #setup for taking max (for labeling purposes) of cum and targets with mutate_
    var_name <- "label_y"
    fcn <- paste0("max(", curr_cum, ",", curr_targets, ")")
  
  #if using a mechanism, filter first for only that mechanism
    if(!is.null(mechid)) df <- dplyr::filter(df, mechanismid == mechid)
  
  #setup table to graph
    df %>% 
      dplyr::filter(indicator == ind,
                    standardizeddisaggregate != "Total Numerator") %>% #want Age/Sex or both Modality/Age Aggregated/Sex/Result & Modality/Age/Sex/Result
      #aggregate up cumulative and target variables
      dplyr::group_by(agesemifine) %>% 
      dplyr::summarise_at(dplyr::vars(curr_cum, curr_targets), ~ sum(., na.rm = TRUE)) %>% 
      dplyr::ungroup() %>% 
      #calculate achievement
      rw_calc_achievement(curr_fy) %>% 
      dplyr::arrange(agesemifine) %>%
      #create labels column for graph, bases on further bar
      dplyr::group_by(agesemifine) %>% 
      dplyr::mutate_(.dots = setNames(fcn, var_name)) %>% 
      dplyr::ungroup() %>% 
      #arrange age descending for factor order in order to graph correctly
      dplyr::arrange(desc(agesemifine)) %>% 
      #adjust missing or large achievement (throws off labels) & factor ages for odering graph 
      dplyr::mutate(achievement = ifelse(is.na(achievement), 1, achievement),
                    label_y = ifelse((achievement > 5 | fy2018_targets == 0), NA, label_y), 
                    achievement = ifelse(achievement > 5, 1, achievement),
                    agesemifine = forcats::as_factor(agesemifine)) %>% 
      dplyr::arrange(agesemifine)  
}




  





  