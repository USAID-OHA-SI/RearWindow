#' Prepare Multiplot for each partner's underperforming mechanism
#'
#' @param df data frame to use
#' @param ind indicator, eg "HTS_TST"
#' @param mech implementing mechanism, eg "21212"
#' @param savepath if specified, folder path where you want the png file save, default = NULL
#'
#' @importFrom dplyr %>%
#' @export

rw_output_mech <- function(df, ind, mech, savepath = NULL){
  
  #plot result trends
    v1 <- rw_plot_trend(df, ind, mech)
    
  #plot targets
    v2 <- rw_plot_trend(df, ind, mech, target = TRUE)
  
  #if additional slide is needed, eg HTS_TST_POS needs HTS_TST
    if(stringr::str_detect(ind, "_POS")) {
      v0 <- rw_plot_trend(df, stringr::str_remove(ind, "_POS"), mech)
    }
  
  #create multiplot with either 2 or 3 columns depending on what's needed
    if(stringr::str_detect(ind, "_POS")){
      output <- gridExtra::grid.arrange(v0, v1, v2, ncol = 3)
    } else {
      output <- gridExtra::grid.arrange(v1, v2, ncol = 2)
    }
  
  #save output
    if(!is.null(savepath)) {
      ou <- unique(df$operatingunit)
      pd <- ICPIutilities::identifypd(df) %>% toupper()
      rw_save(output, savepath, ou, pd, "trend", mech, ind)
    }
  
}
  