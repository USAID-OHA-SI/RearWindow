#' Save visuals
#'
#' @param folderpath path to folder where you want this output stored 
#' @param ou operatingunit
#' @param pd period
#' @param plotname name of the visual
#' @param mech mechanism id (character),, default = NULL
#' @param ind indicator, default = NULL
#' @param w width (in), default = 9 
#' @param h height (in), default = 3.75
#'
#' @export

rw_save <- function(folderpath, ou, pd, plotname = NULL, mech = NULL, ind = NULL, w = 9, h = 3.75){
  
  #define basic save name
    save_name <- paste(ou, pd, sep = "_")
  #if mechanism or indicator exist, include that in save name
    if(!is.null(plotname)) save_name <- paste(save_name, plotname, sep = "_")
    if(!is.null(mech)) save_name <- paste(save_name, mech, sep = "_")
    if(!is.null(ind))  save_name <- paste(save_name, ind, sep = "_")
  #add file extension
    save_name <- paste0(save_name, ".png")
  
  #save
    ggplot2::ggsave(save_name, 
                    path = folderpath,
                    width = w , height = h, units = "in",
                    dpi = 300)
}
  
  