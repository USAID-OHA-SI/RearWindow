#' Graph agency comparison
#'
#' @param df dataframe to use 
#'
#' @importFrom dplyr %>%
#' @export
#'

rw_plot_achv_agency <- function(df){
  
  #add palette
    color <- rw_addpalette()
  
  #establish thresholds
    threshold <- rw_addthresholds(df)
  
  #setup table to graph
  ach <- rw_prep_achv_agency(df)
  
  #graph achievement
  ach %>% 
    ggplot2::ggplot(ggplot2::aes(reorder(indicator, -achievement), achievement)) + 
    ggplot2::geom_hline(yintercept = c(threshold["low"], threshold["med"], threshold["ach"]),
                        color = color["lgray"]) +
    ggplot2::geom_point(ggplot2::aes(y = CDC),
                        na.rm = TRUE,
                        shape = 21,
                        fill = "white",
                        color = color["ubuntu"],
                        stroke = 2,
                        size = 4) +
    ggplot2::geom_point(ggplot2::aes(y = USAID),
                        na.rm = TRUE,
                        shape = 21,
                        fill = color["ice"],
                        color = color["ice"],
                        stroke = 2,
                        size = 4) +
    ggplot2::geom_text(ggplot2::aes(y = CDC, label = scales::percent(CDC)),
                       hjust= ifelse(ach$CDC > ach$USAID, -0.5, 1.5),
                       vjust=.3, 
                       color = color["ubuntu"],
                       na.rm = TRUE) +
    ggplot2::geom_text(ggplot2::aes(y = USAID, label = scales::percent(USAID)),
                       hjust= ifelse(ach$USAID > ach$CDC, -0.5, 1.5),
                       vjust=.3, 
                       color = color["ice"],
                       na.rm = TRUE) +
    ggplot2::geom_text(ggplot2::aes(y = CDC),
                       label = ifelse(ach$lab_CDC == 1, "CDC", NA),
                       vjust = -1,
                       hjust = .4,
                       color = color["txtgray"],
                       na.rm = TRUE) +
    ggplot2::geom_text(ggplot2::aes(y = USAID),
                       label = ifelse(ach$lab_USAID == 1, "USAID", NA),
                       vjust = -1,
                       hjust = .4,
                       color = color["txtgray"],
                       na.rm = TRUE) +
    ggplot2::coord_flip() +
    ggplot2::xlab("") +
    ggplot2::ylab("") +
    rw_plot_theme() +
    ggplot2::theme(axis.text.x = ggplot2::element_blank())
}