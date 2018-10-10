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
  
  #clean with ICPI utilities - offical names, add net new and have FY18 cum
    df <- df %>%
      ICPIutilities::rename_official() %>%
      ICPIutilities::combine_netnew(folderpath_archivedmsd) %>%
      
    #select key columns
    df <- df %>% 
      dplyr::select(operatingunit, fundingagency, mechanismid, implementingmechanismname, primepartner, indicator, 
                    standardizeddisaggregate, agefine, sex, otherdisaggregate, modality, 
                    dplyr::starts_with(fy))
  #add cumulative
    df <- ICPIutilities::add_cumulative(df)
    
  #remove rows with no data to reduce row count
    df <- dplyr::filter_if(df, is.numeric, dplyr::any_vars(!is.na(.) & . != 0))
}