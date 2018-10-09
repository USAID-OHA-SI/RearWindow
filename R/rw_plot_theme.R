#' Setup ggplot theme
#'
#' @export
#'

rw_plot_theme <- function(){
  ggplot2::theme_light() +
  ggplot2::theme(
      legend.position    = "none",
      panel.grid.major   = ggplot2::element_blank(),
      panel.grid.minor   = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_blank(),
      panel.border       = ggplot2::element_blank(),
      #axis.text.x        = ggplot2::element_blank(),
      axis.ticks.x       = ggplot2::element_blank(),
      axis.ticks.y       = ggplot2::element_blank(),
      axis.line.x        = ggplot2::element_line(c_lgray)
  )
}