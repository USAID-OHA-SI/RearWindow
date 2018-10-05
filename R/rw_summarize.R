#' Summarize numeric data
#'
#' @param df data frame to aggregate
#' @param ... list of variables to group collapse/aggregate on
#'
#' @export
#' @importFrom dplyr %>%


rw_summarize <- function(df, ...){
  
  #capture grouping variables as a list of formulas
    group_vars <- dplyr::quos(...)
  
  #aggregate up numeric variables based on specificed grouping
    df <- df %>% 
      dplyr::group_by(!!!group_vars) %>%
      dplyr::summarise_if(is.numeric, ~ sum(., na.rm = TRUE)) %>%
      dplyr::ungroup()
    
    return(df)
}