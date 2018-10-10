#' Plot achievement
#'
#' @param df data frame to plot
#'
#' @importFrom dplyr %>%
#' @importFrom ggplot2 aes
#' @export

rw_plot_achv <- function(df){
  
  #setup table to graph
  ach <- rw_prep_achv(df)
  
  ach %>% 
    ggplot2::ggplot(aes(reorder(indicator, -achievement), achievement)) + 
    ggplot2::geom_hline(yintercept = c(thres_low, thres_med, thres_ach), color = c_lgray) +
    ggplot2::geom_segment(aes(xend=indicator, y=0, yend=achievement), 
                          color= dplyr::case_when(ach$achievement < thres_low  ~ c_ubuntu,
                                                  ach$achievement < thres_med  ~ c_amazon,
                                                  TRUE                         ~ c_grullo), 
                          size= 2) +
    ggplot2::geom_point(color = dplyr::case_when(ach$achievement < thres_low  ~ c_ubuntu,
                                                 ach$achievement < thres_med  ~ c_amazon,
                                                 TRUE                         ~ c_grullo), 
                        size = 6) +
    ggplot2::geom_text(aes(label = scales::percent(achievement)), 
                       hjust=-.5, vjust=.3, color = c_txtgray) +
    ggplot2::coord_flip() +
    ggplot2::xlab("") +
    ggplot2::ylab("") +
    rw_plot_theme() +
    ggplot2::theme(axis.text.x = ggplot2::element_blank())
}