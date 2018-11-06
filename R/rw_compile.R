#' Setup dataset prior 
#'
#' @param filepath filepath to the MSD (rds file)
#' @param opunit operatingunit
#'
#' @export
#' @importFrom dplyr %>%

rw_compile <- function(filepath, opunit){
  
  #import data
    df <- readr::read_rds(filepath) 
    
  #filter ou and select relevant indicators
    df <- rw_pairdown(df, opunit)
  
  #clean with ICPI utilities - offical names
    df <- ICPIutilities::rename_official(df)
      
  #select key columns
    df <- df %>% 
      dplyr::select(operatingunit, fundingagency, mechanismid, implementingmechanismname, primepartner, indicator, 
                    standardizeddisaggregate, agesemifine, sex, otherdisaggregate, modality, 
                    dplyr::starts_with("fy"))
    
  #add in net new target
    df <- rw_gen_nn_target(df)
    
  #add aggregate & add cumulative
    df <- ICPIutilities::add_cumulative(df)
    
  #remove rows with no data to reduce row count
    df <- dplyr::filter_if(df, is.numeric, dplyr::any_vars(!is.na(.) & . != 0))
}