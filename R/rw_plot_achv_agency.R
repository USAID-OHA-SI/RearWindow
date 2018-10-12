#' Graph agency comparison
#'
#' @param df dataframe to use 
#'
#' @importFrom dplyr %>%
#' @export
#'

rw_plot_achv_agency <- function(df){
  
  #establish thresholds
  qtr <- ICPIutilities::identifypd(df, "quarter")
  thres_low <- rw_pull_threshold(qtr, "low")
  thres_med <- rw_pull_threshold(qtr, "med")
  thres_ach <- 1L
  
  #setup table to graph
  ach <- rw_prep_achv_agency(df)
  
  #graph achievement
  ach %>% 
    ggplot2::ggplot(ggplot2::aes(reorder(indicator, -achievement), achievement)) + 
    ggplot2::geom_hline(yintercept = c(thres_low, thres_med, thres_ach), color = c_lgray) +
    ggplot2::geom_point(ggplot2::aes(y = CDC),
                        na.rm = TRUE,
                        shape = 21,
                        fill = "white",
                        color = c_ubuntu,
                        stroke = 2,
                        size = 4) +
    ggplot2::geom_point(ggplot2::aes(y = USAID),
                        na.rm = TRUE,
                        shape = 21,
                        fill = c_ice,
                        color = c_ice,
                        stroke = 2,
                        size = 4) +
    ggplot2::geom_text(ggplot2::aes(y = CDC, label = scales::percent(CDC)),
                       hjust= ifelse(ach$CDC > ach$USAID, -0.5, 1.5),
                       vjust=.3, 
                       color = c_ubuntu,
                       na.rm = TRUE) +
    ggplot2::geom_text(ggplot2::aes(y = USAID, label = scales::percent(USAID)),
                       hjust= ifelse(ach$USAID > ach$CDC, -0.5, 1.5),
                       vjust=.3, 
                       color = c_ice,
                       na.rm = TRUE) +
    ggplot2::geom_text(ggplot2::aes(y = CDC),
                       label = ifelse(ach$lab_CDC == 1, "CDC", NA),
                       vjust = -1,
                       hjust = .4,
                       color = c_txtgray,
                       na.rm = TRUE) +
    ggplot2::geom_text(ggplot2::aes(y = USAID),
                       label = ifelse(ach$lab_USAID == 1, "USAID", NA),
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