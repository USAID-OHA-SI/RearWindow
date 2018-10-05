#' Plot mechanism's indicator quarterly trend
#'
#' @param df data frame to use
#' @param ind indicator plot
#' @param mechid mechanism of interest
#' @param target graph target data? default = FALSE
#'
#' @export
#'


rw_plot_trend <- function(df, ind, mechid, target = FALSE){
  
  #subgrp <- rw_plot_trend(df, ind, mechid)
  
  subt <- dplyr::case_when(ind == "HTS_TST"      ~ "testing volume",
                           ind == "HTS_TST_POS"  ~ "positive testing volume",
                           ind == "TX_NEW"       ~ "treatment volume",
                           TRUE                  ~ "")
  
  if(target == FALSE){
    subgrp <- dplyr::filter(subgrp, qtr != "TARGETS")
    
    subgrp %>% 
      ggplot2::ggplot(ggplot2::aes(qtr, val, group = fy)) +
      ggplot2::geom_line(color = ifelse(subgrp$fy == "FY18", c_ice, c_grullo),
                size = 1) +
      ggplot2::geom_point(shape = 21,
                 fill = ifelse(subgrp$fy == "FY18", c_ice, c_grullo),
                 color = "white",
                 stroke = 2,
                 size = 4) +
      ggplot2::expand_limits(y = 0) +
      ggplot2::scale_y_continuous(label=scales::comma) +
      ggplot2::labs(title = ind, subtitle = subt, x = "", y = "") +
      ggplot2::theme_bw() +
      rw_plot_theme()
    
  } else {
    
    subgrp <- dplyr::filter(subgrp, qtr == "TARGETS")
    
    subgrp %>%
      ggplot2::ggplot(ggplot2::aes(fy, val)) +
      ggplot2::geom_bar(stat = "identity",
                        fill = ifelse(subgrp$fy == "FY18", c_ice, c_grullo)) +
      ggplot2::geom_text(ggplot2::aes(label = scales::comma(val)),
                         vjust = 1.5,
                         color = ifelse(subgrp$fy == "FY18", "white", "black")) +
      ggplot2::labs(title = paste(ind, "Target"), subtitle = paste("targets", subt), x = "", y = "") +
      ggplot2::theme_bw() +
      #thm +
      ggplot2::theme(axis.text.y= ggplot2::element_blank())
  }
}