
rw_plot_achv_age <- function(df, ind, mechid = NULL){
  
  #establish thresholds
    qtr <- ICPIutilities::identifypd(df, "quarter")
    thres_med <- rw_pull_threshold(qtr, "med")
  
  #prep table
    df_age <- rw_prep_achv_age(df, ind, mechid)
  
  #graph  
    df_age %>% 
      ggplot2::ggplot() +
      ggplot2::geom_col(ggplot2::aes(agesemifine, fy2018_targets),  
                        width = 0.8,
                        fill = ifelse(df_age$achievement < thres_med, c_lubuntu, c_lice)) +
      ggplot2::geom_col(ggplot2::aes(agesemifine, fy2018cum),  
                        width = 0.4,
                        fill = ifelse(df_age$achievement < thres_med, c_ubuntu, c_ice)) +
      ggplot2::coord_flip() +
      ggplot2::scale_y_continuous(label=scales::comma) +
      ggplot2::labs(x = "", y = "") +
      ggplot2::geom_text(ggplot2::aes(agesemifine, label_y, label = scales::percent(achievement)),
                         hjust=-.5, vjust=.35, color = c_txtgray) +
      rw_plot_theme() +
      ggplot2::theme(
        axis.line.x        = ggplot2::element_blank(),
        panel.grid.major.x = ggplot2::element_line(c_lgray)
      )  
}

