#' Setup ggplot theme
#'
#' @export
#'

rw_plot_theme <- function(){
  theme_light() %+replace%
    theme(
      legend.position="none",
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.border = element_blank(),
      axis.text.x=element_blank(),
      axis.ticks.x = element_blank(),
      axis.ticks.y = element_blank())
}