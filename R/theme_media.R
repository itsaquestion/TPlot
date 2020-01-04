#' theme_media
#'
#' @param ... argument pass to theme_bw()
#' @return a gg theme
#' @export
#'
theme_media = function(...){
  theme_bw(...) +
    theme(panel.border = element_blank(),
          panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank(),
          axis.line.x =  element_line(colour = "gray"),
          axis.ticks.y = element_blank()) +
    theme(legend.title = element_blank()) + theme(legend.position="bottom")
  
}