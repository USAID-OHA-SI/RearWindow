#' Plot achievement for HTS modalities
#'
#' @param df data frame to plot
#'
#' @importFrom dplyr %>%
#' @importFrom ggplot2 aes
#' @importFrom stats reorder
#' @export

rw_plot_achv_hts <- function(df, ind){
  
  #establish thresholds
    qtr <- ICPIutilities::identifypd(df, "quarter")
    thres_low <- rw_pull_threshold(qtr, "low")
    thres_med <- rw_pull_threshold(qtr, "med")
    thres_ach <- 1L
  
  #setup table to graph
    ach <- rw_prep_achv(df, modality, ind = ind)
  
  #graph achievement
    ach %>% 
      ggplot2::ggplot(aes(reorder(modality, -achievement), achievement)) + 
      ggplot2::geom_hline(yintercept = c(thres_low, thres_med, thres_ach), color = c_lgray) +
      ggplot2::geom_segment(aes(xend=modality, y=0, yend=achievement), 
                            color= dplyr::case_when(ach$achievement < thres_low  ~ c_ubuntu,
                                                    ach$achievement < thres_med  ~ c_amazon,
                                                    TRUE                         ~ c_grullo), 
                            size= 2,
                            na.rm = TRUE) +
      ggplot2::geom_point(color = dplyr::case_when(ach$achievement < thres_low  ~ c_ubuntu,
                                                   ach$achievement < thres_med  ~ c_amazon,
                                                   TRUE                         ~ c_grullo), 
                          size = 6,
                          na.rm = TRUE) +
      ggplot2::geom_text(aes(label = scales::percent(achievement)), 
                         hjust=-.5, vjust=.3, color = c_txtgray,
                         na.rm = TRUE) +
      ggplot2::coord_flip() +
      ggplot2::xlab("") +
      ggplot2::ylab("") +
      rw_plot_theme() +
      ggplot2::theme(axis.text.x = ggplot2::element_blank())
}