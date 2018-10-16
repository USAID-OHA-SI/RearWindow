#' Read in color palette
#' @description  Colors from the ICPI color palette - ICPIutilities::add_color(palette = "woods")
#' @export

rw_addpalette <- function(){
  
  #define palette (for alignment only)
    palette <- 
      tibble::tribble(
              ~name,   ~color,
              "ice", "#335b8e",
             "mint", "#6ca18f",
            "khaki", "#b5b867",
           "ubuntu", "#cc5234",
           "amazon", "#d9812c",
           "grullo", "#948d79",
             "lice", "#a2bcdd", #60% ice
          "lubuntu", "#ebbaae", #60% ubuntu
            "lgray", "#bfbfbf",
          "txtgray", "#404040"
        )

  #extract colors as vector
    color <- dplyr::pull(palette, color)
    
  #apply names to each color (named vector)
    names(color) <- dplyr::pull(palette, name)
  
  return(color)
}