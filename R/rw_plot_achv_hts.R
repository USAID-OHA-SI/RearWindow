#' Plot achievement for HTS modalities
#'
#' @param df data frame to plot
#' @param ind indicator to specify, "HTS_TST" or "HTS_TST_POS"
#' @param agency can specify agency to filter to, eg "USAID" or "CDC", default == NULL
#' 
#' @importFrom dplyr %>%
#' @importFrom ggplot2 aes
#' @importFrom stats reorder
#' @export

rw_plot_achv_hts <- function(df, ind, agency = NULL){
  
  #add palette
    color <- rw_addpalette()
  
  #establish thresholds
    threshold <- rw_addthresholds(df)
  
  #setup table to graph
    ach <- rw_prep_achv(df, modality, ind = ind, agency = agency)
  
  #graph achievement
    ach %>% 
      ggplot2::ggplot(aes(reorder(modality, -achievement), achievement)) + 
      ggplot2::geom_hline(yintercept = c(threshold["low"], threshold["med"], threshold["ach"]), 
                          color = color["lgray"]) +
      ggplot2::geom_segment(aes(xend=modality, y=0, yend=achievement), 
                            color= dplyr::case_when(ach$achievement < threshold["low"]  ~ color["ubuntu"],
                                                    ach$achievement < threshold["med"]  ~ color["amazon"],
                                                    TRUE                                ~ color["grullo"]), 
                            size= 2,
                            na.rm = TRUE) +
      ggplot2::geom_point(color = dplyr::case_when(ach$achievement < threshold["low"]  ~ color["ubuntu"],
                                                   ach$achievement < threshold["med"]  ~ color["amazon"],
                                                   TRUE                                ~ color["grullo"]), 
                          size = 6,
                          na.rm = TRUE) +
      ggplot2::geom_text(aes(label = scales::percent(achievement)), 
                         hjust=-.5, vjust=.3, color = color["txtgray"],
                         na.rm = TRUE) +
      ggplot2::coord_flip() +
      ggplot2::xlab("") +
      ggplot2::ylab("") +
      rw_plot_theme() +
      ggplot2::theme(axis.text.x = ggplot2::element_blank())
}