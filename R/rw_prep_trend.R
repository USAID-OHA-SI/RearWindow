
#' Reshape selected mechanism/indicator data long
#'
#' @param df data frame to use
#' @param ind indicator selection
#' @param mechid mechanism to filter to
#'
#' @importFrom dplyr %>%
#' @export

rw_prep_trend <- function(df, ind, mechid){
  
  df %>%
    dplyr::filter(indicator == ind,
                  standardizeddisaggregate == "Total Numerator",
                  mechanismid == mechid) %>%
    dplyr::group_by(mechanismid, indicator) %>%
    dplyr::summarise_at(dplyr::vars(dplyr::contains("q"), dplyr::ends_with("targets")), 
                        ~ sum(., na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    tidyr::gather(pd, val, -mechanismid, -indicator) %>%
    # spread(indicator, val) %>%
    tidyr::separate(pd, c("fy", "qtr"), 6, remove = FALSE) %>% 
    dplyr::mutate(fy = stringr::str_remove(fy, "20") %>% toupper(),
           qtr = stringr::str_remove(qtr, "_") %>% toupper(),
           qtr_num = ifelse(qtr!="TARGETS", stringr::str_remove(qtr, "Q"), 0),
           fylabel = ifelse(qtr_num == ICPIutilities::identifypd(df, "quarter"), fy, "")) %>% 
    dplyr::filter(qtr_num <= ICPIutilities::identifypd(df, "quarter")) %>% 
    dplyr::select(-qtr_num)
}