
#' Plot a table of all operating unit's mechanisms and their indicator achievement
#'
#' @param df 
#'
#' @export
#' @importFrom dplyr %>% 

rw_plot_mech_table <- function(df){
  
  #add palette
  color <- rw_addpalette()
  
  #establish thresholds
  threshold <- rw_addthresholds(df)
  
  #setup table to graph
  df_mech <- rw_prep_mech_table(df)
  
  #add list of mechs and inds w/ lower than expected achievement
  df_low <- df_mech %>%
    dplyr::mutate(indicator = as.character(df_mech$indicator)) %>% 
    dplyr::filter(achievement < threshold["med"]) 
  lst_mech <- unique(df_low$mechanismid)
  lst_ind <- unique(df_low$indicator)
  rm(df_low)
  
  
  #plot table
  df_mech %>% 
    ggplot2::ggplot(ggplot2::aes(x = reorder(mechanismid, -ind_count), y = indicator)) +
    ggplot2::geom_point(
      size = 6,
      color= dplyr::case_when(df_mech$achievement < threshold["low"]  ~ color["ubuntu"],
                              df_mech$achievement < threshold["med"]  ~ color["amazon"],
                              TRUE                               ~ color["lmint"])
    ) +
    ggplot2::geom_text(ggplot2::aes(label = scales::percent(achievement)), 
                       hjust=1.3, vjust=.4, 
                       color = dplyr::case_when(df_mech$achievement < threshold["med"]  ~ "black",
                                                TRUE                               ~ color["txtgray2"]),
                       size = 5,
                       na.rm = TRUE) +
    ggplot2::labs(x = "", y = "") +
    ggplot2::scale_x_discrete(position = "top") + 
    rw_plot_theme() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(hjust = .9,
                                          face = ifelse(df_mech$mechanismid %in% lst_mech, "bold", "plain"),
                                          size = 16),
      axis.text.y = ggplot2::element_text(face = ifelse(df_mech$indicator %in% lst_ind, "bold", "plain"),
                                          size = 14),
      axis.line.x = ggplot2::element_blank(),
      axis.line.y = ggplot2::element_line(color["lgray"]))
  
}
