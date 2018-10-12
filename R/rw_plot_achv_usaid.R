#' Plot achievement for USAID vs National
#'
#' @param df data frame to plot
#'
#' @importFrom dplyr %>%
#' @importFrom ggplot2 aes
#' @importFrom stats reorder
#' @export

rw_plot_achv_usaid <- function(df){
  
  #establish thresholds
  qtr <- ICPIutilities::identifypd(df, "quarter")
  thres_low <- rw_pull_threshold(qtr, "low")
  thres_med <- rw_pull_threshold(qtr, "med")
  thres_ach <- 1L
  
  #setup table to graph
  ach <- rw_prep_achv_agency(df)
  
  #graph achievement
  ach %>% 
    ggplot2::ggplot(aes(reorder(indicator, -achievement), achievement)) + 
    ggplot2::geom_hline(yintercept = c(thres_low, thres_med, thres_ach), color = c_lgray) +
    ggplot2::geom_segment(aes(xend=indicator, y=0, yend=USAID), 
                          color= dplyr::case_when(ach$USAID < thres_low  ~ c_ubuntu,
                                                  ach$USAID < thres_med  ~ c_amazon,
                                                  TRUE                   ~ c_grullo), 
                          size= 2,
                          na.rm = TRUE) +
    ggplot2::geom_point(aes(y = achievement), 
                        shape = 21,
                        fill = "white",
                        color = dplyr::case_when(ach$achievement < thres_low  ~ c_ubuntu,
                                                 ach$achievement < thres_med  ~ c_amazon,
                                                 TRUE                         ~ c_grullo), 
                        stroke = 2,
                        size = 4,
                        na.rm = TRUE) +
    ggplot2::geom_point(aes(y = USAID), 
                        color = dplyr::case_when(ach$USAID < thres_low  ~ c_ubuntu,
                                                 ach$USAID < thres_med  ~ c_amazon,
                                                 TRUE                   ~ c_grullo), 
                        size = 6,
                        na.rm = TRUE) +
    ggplot2::geom_text(aes(y = USAID, label = scales::percent(USAID)), 
                       hjust=-.5, vjust=.3, color = c_txtgray,
                       na.rm = TRUE) +
    ggplot2::geom_text(ggplot2::aes(y = USAID),
                       label = ifelse(ach$lab_USAID == 1, "USAID", NA),
                       vjust = -1,
                       hjust = .4,
                       color = c_txtgray,
                       na.rm = TRUE) +
    ggplot2::geom_text(ggplot2::aes(y = achievement),
                       label = ifelse(ach$lab_NAT == 1, "National", NA),
                       vjust = -1,
                       hjust = .4,
                       color = c_txtgray,
                       na.rm = TRUE) +
    ggplot2::coord_flip() +
    ggplot2::xlab("") +
    ggplot2::ylab("") +
    rw_plot_theme() +
    ggplot2::theme(axis.text.x = ggplot2::element_blank())
}