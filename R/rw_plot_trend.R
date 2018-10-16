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
  
  subgrp <- rw_prep_trend(df, ind, mechid)
  
  subt <- dplyr::case_when(ind == "HTS_TST"        ~ "testing volume",
                           ind == "HTS_TST_POS"    ~ "positive testing volume",
                           ind == "TX_NEW"         ~ "new on treatment volume",
                           ind == "VMMC_CIRC"      ~ "voluntary MC volume",
                           ind == "TX_CURR"        ~ "current on treatment volume",
                           ind == "TX_NET_NEW"     ~ "net new on treatment volume",
                           ind == "PrEP_NEW"       ~ "PrEP volume",
                           ind == "PMTCT_STAT_POS" ~ "postive PMTCT volume",
                           ind == "PMTCT_STAT"     ~ "PMTCT volume",
                           ind == "PMTCT_EID"      ~ "PMTCT_EID volume",
                           TRUE                    ~ "volume")
  
  if(target == FALSE){
    subgrp <- dplyr::filter(subgrp, qtr != "TARGETS")
    
    subgrp %>% 
      ggplot2::ggplot(ggplot2::aes(qtr, val, group = fy)) +
      ggplot2::geom_line(color = ifelse(subgrp$fy == "FY18", color["ice"], color["grullo"]),
                size = 1) +
      ggplot2::geom_point(shape = 21,
                 fill = ifelse(subgrp$fy == "FY18", color["ice"], color["grullo"]),
                 color = "white",
                 stroke = 2,
                 size = 4) +
      ggplot2::geom_text(aes(label = fylabel), size = 3,
                         hjust=-.5, color = color["txtgray"]) +
      ggplot2::expand_limits(y = 0) +
      ggplot2::scale_y_continuous(label=scales::comma) +
      ggplot2::labs(title = ind, subtitle = subt, caption = "", x = "", y = "") +
      ggplot2::theme_bw() +
      rw_plot_theme()
    
  } else {
    
    subgrp <- dplyr::filter(subgrp, qtr == "TARGETS")
    
    subgrp %>%
      ggplot2::ggplot(ggplot2::aes(fy, val)) +
      ggplot2::geom_bar(stat = "identity",
                        fill = dplyr::case_when(subgrp$fy == "FY18" ~ color["ice"], 
                                                subgrp$fy == "FY19" ~ color["mint"],
                                                TRUE                ~ color["grullo"])) +
      ggplot2::geom_text(ggplot2::aes(label = scales::comma(val)),
                         vjust = 1.5,
                         fontface = "bold",
                         color = ifelse(subgrp$fy == "FY17", "black", "white")) +
      ggplot2::labs(title = paste(ind, "Target"), subtitle = paste("targets", subt),
                    caption = mechid, x = "", y = "") +
      ggplot2::theme_bw() +
      rw_plot_theme() + 
      ggplot2::theme(axis.text.y= ggplot2::element_blank())
  }
}