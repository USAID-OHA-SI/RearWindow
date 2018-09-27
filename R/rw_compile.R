#' Setup dataset prior 
#'
#' @param filepath filepath to the MSD (rds file)
#' @param opunit operatingunit
#' @param folderpath_archivedmsd folderpath for archived MSD dataset, used for creating TX_NET_NEW
#'
#' @export
#' @importFrom dplyr %>%

rw_compile <- function(filepath, opunit, folderpath_archivedmsd = NULL){
  
  #import data
    df <- readr::read_rds(filepath) 
    
  #filter ou and select relevant indicators
    df <- rw_pairdown(df, opunit)
  
  #identify current year cum and targets
    curr_fy <- ICPIutilities::identifypd(df, pd_type = "year")
    curr_cum <- paste0("fy", curr_fy, "cum")
    curr_targets <- ICPIutilities::identifypd(df, pd_type = "target")
    
  #clean with ICPI utilities - offical names, add net new and have FY18 cum
    df <- df %>%
      ICPIutilities::rename_official() %>%
      ICPIutilities::combine_netnew(folderpath_archivedmsd) %>%
      ICPIutilities::add_cumulative() 
  
  #select key columns
    df <- df %>% 
      dplyr::select(operatingunit, mechanismid, implementingmechanismname, primepartner, indicator, 
                    standardizeddisaggregate, agefine, sex, otherdisaggregate, modality, 
                    curr_cum, curr_targets)
    
}