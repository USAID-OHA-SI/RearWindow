
#' Plot age achievement for a given indicator
#'
#' @param df data frame to use
#' @param ind indicator to graph
#' @param mechid if interested in a particular mechanism, eg "82182"; default = NULL
#' @param agency can specify agency, eg "USAID" or "CDC", default = NULL
#'
#' @importFrom dplyr %>% 
#' @export


rw_plot_achv_age <- function(df, ind, mechid = NULL, agency = NULL){
  
  #add palette
    color <- rw_addpalette()
  
  #establish thresholds
    threshold <- rw_addthresholds(df)
  
  #prep table
    df_age <- rw_prep_achv_age(df, ind, mechid, agency)
  
  #graph  
    df_age %>% 
      ggplot2::ggplot() +
      ggplot2::geom_col(ggplot2::aes(agesemifine, fy2018_targets),  
                        width = 0.8,
                        fill = ifelse(df_age$achievement < threshold["med"], color["lubuntu"], color["lice"]),
                        na.rm = TRUE) +
      ggplot2::geom_col(ggplot2::aes(agesemifine, fy2018cum),  
                        width = 0.4,
                        fill = ifelse(df_age$achievement < threshold["med"], color["ubuntu"], color["ice"]),
                        na.rm = TRUE) +
      ggplot2::coord_flip() +
      ggplot2::scale_y_continuous(label=scales::comma) +
      ggplot2::labs(x = "", y = "") +
      ggplot2::geom_text(ggplot2::aes(agesemifine, label_y, label = scales::percent(achievement)),
                         hjust=-.5, vjust=.35, color = color["txtgray"],
                         na.rm = TRUE) +
      rw_plot_theme() +
      ggplot2::theme(
        axis.line.x        = ggplot2::element_blank(),
        panel.grid.major.x = ggplot2::element_line(color["lgray"])
      )  
}

